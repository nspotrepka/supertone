(ns supertone.midi.launchpad-mk2
  (:require [overtone.libs.event     :refer :all]
            [overtone.studio.midi    :refer :all]
            [supertone.studio.midi   :as midi]))

(def indices {
  :grid  (into [] (flatten (map #(range (+ 11 %) (+ 19 %)) (range 0 80 10))))
  :top   (into [] (range 104 112))
  :right (into [] (range 19 99 10))})

(def colors {
  :gray   [0 1 2 3]
  :light  [4 8 12 20 36 44 48 52]
  :normal [5 9 13 21 37 45 49 53]
  :dark   [7 11 15 23 39 47 51 55]})

(defn- generate-symbol
  []
  (gensym "launchpad_mk2_"))

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

(defn set-rgb
  "Sets RGB value on controller."
  ([rcv section index color-type color-index]
    (set-rgb rcv section index color-type color-index 0))
  ([rcv section index color-type color-index action]
  (let [led (get (get indices section) index)
        color (get (get colors color-type) color-index)]
    (if (= section :top)
      (midi-control rcv led color action)
      (midi-note-on rcv led color action)))))
