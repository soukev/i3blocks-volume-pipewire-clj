# volume-pipewire-clj

This is rewrite of [volume-pipewire](https://github.com/vivien/i3blocks-contrib/tree/master/volume-pipewire) in babashka/clojure, which enables subscribing to audio changes and usage of button events, and also removes dependency on amixer.

## Dependencies
babashka, pipewire-pulse, pipewire-alsa, pipewire-jack, fontawesome (fonts-font-awesome package) for the speaker symbols

## Arguments
```
-S          subscribing mode, subscribes to audio changes and button events
-D<number>  uses <number> of words from 'description' to name active device
```

## Config
```INI
[volume-pipewire-clj]
command=bb $SCRIPT_DIR/volume_pipewire.clj -S -D1
interval=persistent
```
