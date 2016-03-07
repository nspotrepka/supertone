(ns supertone.studio.midi
  (:use overtone.core))

(defn init-midi
  []
  (println "--> Binding midi event handlers..."))

(comment
(defprotocol MidiDevice [])

(defrecord DefaultMidiDevice [])

(defmacro midi-device
  "A MIDI device"
  []
  ))
