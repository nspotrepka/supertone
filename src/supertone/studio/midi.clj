(ns supertone.studio.midi
  (:require [overtone.libs.event  :refer :all]
            [overtone.studio.midi :refer :all]))

(defn devices
  "Get the connected MIDI devices (midi->computer)."
  []
  (midi-connected-devices))

(defn receivers
  "Get the connected MIDI receivers (computer->midi)."
  []
  (midi-connected-receivers))

(defn add-note-on-handler
  "Adds a note on handler."
  [device-id note handler key]
  (on-event (concat device-id [:note-on note])
    (fn [e]
      (let [note (:note e)
            amp  (:velocity-f e)]
      (handler note amp)))
    key))

(defn add-note-off-handler
  "Adds a note off handler."
  [device-id note handler key]
  (on-event (concat device-id [:note-off note])
    (fn [e]
      (let [note (:note e)
            amp  (:velocity-f e)]
        (handler note amp)))
    key))

(defn add-poly-pressure-handler
  "Adds a poly pressure handler."
  [device-id note handler key]
  (on-event (concat device-id [:poly-pressure note])
    (fn [e]
      (let [index (:data1 e)
            value (:data2-f e)]
        (handler index value)))
    key))

(defn add-control-change-handler
  "Adds a control change handler."
  [device-id control-number handler key]
  (on-event (concat device-id [:control-change control-number])
    (fn [e]
      (let [index (:data1 e)
            value (:data2-f e)]
        (handler index value)))
    key))
