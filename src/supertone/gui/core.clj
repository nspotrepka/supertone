(ns supertone.gui.core
  (:require [quil.core :as q])
  (:use [quil.applet :only [current-applet applet-close]])
  (:import [controlP5 ControlEvent Controller ControllerGroup ControlP5 ControlFont ControlKey Textlabel])
  (:use supertone.gui.util)
  (:use supertone.gui.theme)
  (:use supertone.gui.icons)
  (:use supertone.gui.bus))

(defrecord UserInterface [applet cp5 font cp5-font])

(def gui* (atom (map->UserInterface {})))

(def DEFAULT_TAB_NAME "default")

(defn setup-gui
  []
  (q/no-smooth)
  (q/frame-rate 60)
  (reset! gui* (assoc @gui* :applet (current-applet)))
  (reset! gui* (assoc @gui* :cp5 (ControlP5. (:applet @gui*))))
  (reset! gui* (assoc @gui* :font (q/create-font "pixel_operator/PixelOperator.ttf" 16 true)))
  (reset! gui* (assoc @gui* :cp5-font (ControlFont. (:font @gui*) 16)))
  (-> (:cp5 @gui*)
    (.getWindow)
    (.setPositionOfTabs 0 LINE_HEIGHT))
  (-> (:cp5 @gui*)
    (.addTextlabel "supertone")
    (.setFont (:cp5-font @gui*))
    (.setText "Supertone v0.1.0")
    (.setPosition 0 0)
    (.moveTo "global"))
  (-> (:cp5 @gui*)
    (.getTab "default")
    (.setLabel "about")
    (.setHeight (- LINE_HEIGHT 1))
    (.getCaptionLabel)
    (.setFont (:cp5-font @gui*))
    (.getStyle)
    (.marginTop)
    (set! -2))
  (bus-tab @gui*)
  (apply-theme @gui*))

(defn draw-gui
  []
  (q/background (get-color :b0))
  (when (tab-active? @gui* BUS_TAB_NAME) (draw-bus-tab)))

(defn key-pressed-gui
  []
  (case (q/raw-key)
    \1 (show-tab @gui* DEFAULT_TAB_NAME)
    \2 (show-tab @gui* BUS_TAB_NAME)
    q/CODED (case (q/key-code)
      (set! (.key (quil.applet/current-applet)) (char 0))
      nil)
    nil))

(defn mouse-wheel-gui
  [rotation]
  (when (tab-active? @gui* BUS_TAB_NAME) (mouse-wheel-bus-tab rotation)))

(defn open-gui
  []
  (dispose (:applet @gui*))
  (q/sketch
    :title "Supertone"
    :size [1280 720]
    :features [:resizable]
    :setup setup-gui
    :draw draw-gui
    :key-pressed key-pressed-gui
    :mouse-wheel mouse-wheel-gui))
