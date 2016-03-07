(ns supertone.gui.util
  (:require [quil.core :as q]))

(def LINE_HEIGHT 21)
(def HEADER_HEIGHT 42)
(def TEXT_OFFSET_X 4)
(def TEXT_OFFSET_Y 15)
(def SCROLL_SPEED 4)

(defn dispose
  "Close and dispose of a sketch."
  [applet]
  (when applet
    (let [frame (.frame applet)]
    (.setVisible frame false)
    (.dispose frame))))

(defn get-width
  "Get the width of a sketch."
  [g]
  (get (:size (meta (:applet g))) 0))

(defn get-height
  "Get the height of a sketch."
  [g]
  (get (:size (meta (:applet g))) 1))

(defn add-tab
  "Create a tab."
  [g name]
  (-> (:cp5 g)
    (.addTab name)
    (.setHeight (- LINE_HEIGHT 1))
    (.getCaptionLabel)
    (.setFont (:cp5-font g))
    (.getStyle)
    (.marginTop)
    (set! -2)))

(defn tab-active?
  "Check if a tab is active."
  [g name]
  (.isActive (.getTab (:cp5 g) name)))

(defn show-tab
  "Show a tab of a particular name."
  [g name]
  (-> (:cp5 g)
    (.getTab name)
    (.bringToFront)))

(defn draw-text
  "Draw text with offset."
  [s x y]
  (q/text s (+ x TEXT_OFFSET_X) (+ y TEXT_OFFSET_Y)))
