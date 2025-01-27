#!/usr/bin/env bb

(require '[babashka.process :refer [process shell]]
         '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.core.async :as async])

(def config
  (atom {:audio-high-symbol " "
         :audio-med-thresh 50
         :audio-med-symbol " "
         :audio-low-thresh 0
         :audio-low-symbol " "
         :audio-muted-symbol " "
         :audio-delta 5
         :subscribe false
         :description-name nil}))

(defn match-d-number [input]
  (if-let [[_ number] (re-matches #"-D(\d+)" input)]
    (Integer/parseInt number)
    nil))

(defn parse-args [args]
  (doseq [arg args]
    (let [desc-num (match-d-number arg)]
     (cond
       (= "-S" arg) (swap! config assoc :subscribe true)
       desc-num (swap! config assoc :description-name desc-num)
       :else (println "Unknown option:" arg)))))

(defn set-default-playback-device-next [direction]
  (let [sinks (->> (process ["pactl" "list" "sinks"] {:out :string})
                   deref
                   :out
                   (str/split-lines)
                   (filter #(str/includes? % "Name:"))
                   (map #(second (re-find #"Name: (.+)" %))))]
    (when (seq sinks)
      (let [num-devices (count sinks)
            default-sink (->> (process ["pactl" "get-default-sink"] {:out :string})
                              deref
                              :out
                              str/trim)
            current-index (->> (map-indexed vector sinks)
                               (filter #(= default-sink (second %)))
                               ffirst)
            next-index (let [idx (+ current-index direction)]
                         (cond
                           (>= idx num-devices) 0
                           (< idx 0) (dec num-devices)
                           :else idx))
            new-default (nth sinks next-index)]
        (process ["pactl" "set-default-sink" new-default])))))

(defn find-with-context [lines target before after]
  (let [splitted-lines (str/split-lines lines)
        line-count (count splitted-lines)]
    (->> (map-indexed vector splitted-lines)
         (filter (fn [[_ line]] (re-find (re-pattern target) line)))
         (mapcat (fn [[idx _]]
                   (let [start (max 0 (- idx before))
                         end (min line-count (+ idx after 1))]
                     (subvec splitted-lines start end))))
         (str/join "\n"))))

(defn get-active-sink []
  (let [running (-> (process {:out :string} "pactl list sinks")
                    deref :out
                    (find-with-context "State: Running" 4 55))]
    (if (str/blank? running)
      (let [default-sink (-> (shell {:out :string} "pactl get-default-sink") :out)]
        (-> (process {:out :string} "pactl list sinks")
            deref :out
            (find-with-context (str/trim default-sink) 4 55)))
      running)))

(defn name-from-sink-info
  [sink-info]
  (if (:description-name @config)
    (->> sink-info
         (re-find #"Description: (.*)")
         second
         (#(str/split % #" "))
         (take (:description-name @config))
         (str/join " "))
    (->> sink-info
         (re-find #"node.nick = \"(.*)\"")
         second)))

(defn create-display-string [symb vol index name]
  (let [identificator (if index
                        (str "[" index ":" name "]")
                        (str "[" name "]"))]
    (str symb vol "% " identificator)))

(defn print-block []
  (let [active (get-active-sink)
        vol (->> active
                 (re-find #"Volume: .*?([0-9]+)%")
                 second)
        muted (->> active
                   (re-find #"Mute: (yes|no)")
                   second)
        index (->> active
                   (re-find #"api.alsa.pcm.card = \"([0-9]+)\"")
                   second)
        name (name-from-sink-info active)
        symb (cond
               (= muted "yes") (:audio-muted-symbol @config)
               (<= (Integer. vol) (:audio-low-thresh @config)) (:audio-low-symbol @config)
               (<= (Integer. vol) (:audio-med-thresh @config)) (:audio-med-symbol @config)
               :else (:audio-high-symbol @config))]
    (println (create-display-string symb vol index name))))

(defn mute-default [] (shell "pactl set-sink-mute @DEFAULT_SINK@ toggle"))
(defn volume-default+ [] (shell (format "pactl set-sink-volume @DEFAULT_SINK@ +%s%%" (:audio-delta @config))))
(defn volume-default- [] (shell (format "pactl set-sink-volume @DEFAULT_SINK@ -%s%%" (:audio-delta @config))))

(defn button-action
  [& [{:keys [button] :or {button (System/getenv "BLOCK_BUTTON")}}]]
  (case button
    "1" (set-default-playback-device-next 1)
    "2" (mute-default)
    "3" (set-default-playback-device-next -1)
    "4" (volume-default+)
    "5" (volume-default-)))

(defn run-async-channel! [stream channel channel-key]
  (async/thread
    (try
      (loop []
        (when-let [line (.readLine stream)]
          (async/>!! channel {:source channel-key :line line})
          (recur)))
      (finally
        (async/close! channel)))))

(defn handle-input []
  (let [pactl-proc (process ["pactl" "subscribe"] {:out :pipe})
        pactl-out (io/reader (:out pactl-proc))
        stdin (io/reader System/in)
        pactl-chan (async/chan)
        stdin-chan (async/chan)]
    (run-async-channel! pactl-out pactl-chan :pactl)
    (run-async-channel! stdin stdin-chan :stdin)
    (loop []
      (let [[message _] (async/alts!! [pactl-chan stdin-chan])]
        (when message
          (case (:source message)
            :pactl (when (str/includes? (:line message) "change")
                     (print-block))
            :stdin (button-action {:button (:line message)}))))
      (recur))))

(defn main [& args]
  (parse-args (first args))

  (let [button-press (System/getenv "BLOCK_BUTTON")]
    (when button-press
      (button-action button-press)))

  (print-block)

  (when (:subscribe @config)
    (handle-input)))

(main *command-line-args*)
