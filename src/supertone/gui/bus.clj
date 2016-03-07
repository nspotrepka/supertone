(ns supertone.gui.bus
  (:require [quil.core :as q])
  (:use supertone.gui.util))

(def BUS_TAB_NAME "busses")
(def sidebar-width 200)
(def scroll-y* (atom 0))

(defn bus-tab
  [g]
  (add-tab g BUS_TAB_NAME)
    BUS_TAB_NAME)

(defn draw-bus-tab
  []
  (q/push-matrix)
  (q/translate 0 @scroll-y*)
  (q/rect 44 44 128 128)
  (q/pop-matrix))

(defn mouse-wheel-bus-tab
  [rotation]
    (reset! scroll-y* (+ @scroll-y* (* SCROLL_SPEED rotation))))
