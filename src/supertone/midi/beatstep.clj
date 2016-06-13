(ns supertone.midi.beatstep
  (:require [overtone.libs.event   :refer :all]
            [overtone.studio.midi  :refer :all]
            [supertone.studio.midi :as midi]))

(def indices {
  :jumbo [7]
  :knob  [10 74 71 76 77 93 73 75
          114 18 19 16 17 91 79 72]
  :pad   [44 45 46 47 48 49 50 51
          36 37 38 39 40 41 42 43]})

(defn- generate-symbol
  []
  (gensym "beatstep_"))

(defn note-on
  "Adds a note on handler."
  [dev note handler key]
  (let [device-id (midi-mk-full-device-key dev)]
    (midi/add-note-on-handler device-id note handler key)))

(defn note-off
  "Adds a note off handler."
  [dev note handler key]
  (let [device-id (midi-mk-full-device-key dev)]
    (midi/add-note-off-handler device-id note handler key)))

(defn control-change
  "Adds a note on handler."
  [dev control-number handler key]
  (let [device-id (midi-mk-full-device-key dev)]
    (midi/add-control-change-handler device-id control-number handler key)))

(defn poly-pressure
  "Adds a poly pressure handler."
  [dev note handler key]
  (let [device-id (midi-mk-full-device-key dev)]
    (midi/add-poly-pressure-handler device-id note handler key)))
