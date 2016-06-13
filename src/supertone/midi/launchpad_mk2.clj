(ns supertone.midi.launchpad-mk2
  (:require [overtone.libs.event     :refer :all]
            [overtone.studio.midi    :refer :all]
            [com.evocomputing.colors :refer :all]
            [supertone.studio.midi   :as midi]))

(def indices {
  :grid  (into [] (flatten (map #(range (+ 11 %) (+ 19 %)) (range 0 80 10))))
  :top   (into [] (range 104 112))
  :right (into [] (range 19 99 10))})

(def colors {
  :gray   [0 1 3]
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
  ([rcv section index color-rgb]
    (set-rgb rcv section index color-rgb 0))
  ([rcv section index color-rgb action]
  (let [led (get (get indices section) index)
        hue (hue color-rgb)
        sat (saturation color-rgb)
        lit (lightness color-rgb)
        c-type (if (< sat 50.0)
                 :gray
                 (if (< lit 33.3) :dark (if (< lit 66.6) :normal :light)))
        c-index (if (< sat 50.0)
                  (if (< lit 33.3) 0 (if (< lit 66.6) 1 2))
                  (int (quot (+ 22.5 hue) 45)))
        a (println c-type)
        b (println c-index)
        color (get (get colors c-type) c-index)]
    (if (= section :top)
      (midi-control rcv led color action)
      (midi-note-on rcv led color action)))))
