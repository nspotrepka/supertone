(ns supertone.gui.theme
  (:require [quil.core :as q]))

(defstruct theme :b0 :b1 :b2 :f1 :t1 :t2 :s1 :s2)

(def themes {
  :default (struct theme [240 240 255] [212 212 255] [255 255 255] [200 200 255] [96 96 255] [24 24 32])
})

(def palette (:default themes))

(defn color-rgba
  ([w] (color-rgba w w w 255))
  ([w a] (color-rgba w w w a))
  ([r g b] (color-rgba r g b 255))
  ([r g b a] (bit-or (bit-shift-left a 24) (bit-shift-left r 16) (bit-shift-left g 8) b)))

(defn color [c] (apply color-rgba c))

(defn get-color [s] (color (s palette)))

(defn set-theme
  [t]
  (def palette (t themes)))

(defn apply-theme
  [g]
  (let [cp5 (:cp5 g)]
    (.setColorActive cp5 (get-color :b2))
    (.setColorBackground cp5 (get-color :b1))
    (.setColorCaptionLabel cp5 (get-color :t1))
    (.setColorForeground cp5 (get-color :f1))
    (.setColorValueLabel cp5 (get-color :t2))))
