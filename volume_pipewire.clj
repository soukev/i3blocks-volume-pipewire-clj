#!/usr/bin/env bb

(ns volume-pipewire
  (:require [babashka.process :refer [process shell]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]))

(def config
  (atom {:audio-high-symbol " "
         :audio-med-thresh 50
         :audio-med-symbol " "
         :audio-low-thresh 0
         :audio-low-symbol " "
         :audio-muted-symbol " "
         :audio-delta 5
         :subscribe false}))

(defn parse-args [args]
  (doseq [arg args]
      (case arg
        "-S" (swap! config assoc :subscribe true)
        (println "Unknown option:" arg))))

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

(defn grep-with-context
  [lines target before after]
  (let [splitted-lines (str/split-lines lines)
        line-count (count splitted-lines)]
    (->> (map-indexed vector splitted-lines)
         (filter (fn [[_ line]] (re-find (re-pattern target) line)))
         (mapcat (fn [[idx _]]
                   (let [start (max 0 (- idx before))
                         end (min line-count (+ idx after 1))]
                     (subvec splitted-lines start end))))
         (str/join "\n"))))

(defn get-active-sink
  []
  (let [running (-> (process {:out :string} "pactl list sinks")
                    deref :out
                    (grep-with-context "State: Running" 4 55))]
    (if (str/blank? running)
      (let [default-sink (-> (shell {:out :string} "pactl get-default-sink") :out)]
        (-> (process {:out :string} "pactl list sinks")
            deref :out
            (grep-with-context (str/trim default-sink) 4 55)))
      running)))

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
        nick (->> active
                  (re-find #"node.nick = \"(.*)\"")
                  second)
        symb (cond
               (= muted "yes") (:audio-muted-symbol @config)
               (<= (Integer. vol) (:audio-low-thresh @config)) (:audio-low-symbol @config)
               (<= (Integer. vol) (:audio-med-thresh @config)) (:audio-med-symbol @config)
               :else (:audio-high-symbol @config))]
    (println (str symb vol "%" " [" index ":" nick "]"))))


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

(defn handle-input []
  (println "Listening for pactl events and stdin input (type 1-5):")
  (let [pactl-proc (process ["pactl" "subscribe"] {:out :pipe})
        pactl-out (io/reader (:out pactl-proc))
        stdin (io/reader System/in)]
    (loop []
      (-> (async/alts!! [(async/thread (.readLine pactl-out))
                         ])
          first :out print)
      ;; (let [futures {:pactl (future (try (.readLine pactl-out) (catch Exception _ nil)))
      ;;                  :stdin (future (try (.readLine stdin) (catch Exception _ nil)))}
      ;;       readable (select-keys futures (keys (filter #(realized? (% futures)) '(:pactl :stdin))))]

      ;;   (cond
      ;;     (:pactl readable)
      ;;     (when-let [line (deref (:pactl readable))]
      ;;       (println "Received from pactl:" line))

      ;;     (:stdin readable)
      ;;     (when-let [line (deref (:stdin read))] )))
      (recur))))

(defn main [& args]
  (parse-args (first args))
  (let [button-press (System/getenv "BLOCK_BUTTON")]
    (when button-press
      (button-action button-press)))
  (print-block)

  (when (:subscribe @config)
    (handle-input)))
    ;; (let [pactl-proc (process ["pactl" "subscribe"] {:out :pipe})
    ;;       reader (io/reader (:out pactl-proc))]
    ;;   (loop []
    ;;     (when-let [line (.readLine reader)]
    ;;       (when (str/includes? line "change")
    ;;         (print-block))
    ;;       (recur))))))

(main *command-line-args*)
