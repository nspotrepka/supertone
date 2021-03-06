(ns supertone.view.gui
  (:require [seesaw.core    :refer :all]
            [overtone.sc.node :refer :all]
            [supertone.util :as util]
            [supertone.studio.audio :as audio]
            [supertone.studio.midi :as midi]))

(defrecord GUI [frame controllers])

(def frame* (atom nil))
(def controllers* (atom nil))

(defn- move-to-center
  [frame]
  (.setLocationRelativeTo frame nil))

(defn- move-to-front
  [frame]
  (.setVisible frame true)
  (.setAlwaysOnTop frame true)
  (.toFront frame)
  (.requestFocus frame)
  (.setAlwaysOnTop frame false))

(defn content
  [bg fg font]
  (let [l1 (listbox :model ["Chiptune" "Sinewave" "WhAt"]
                    :background "#888"
                    :foreground :red)
        l2 (listbox :model ["Never" "gonna" "give" "you" "up"])
        l3 (listbox :model (range 0 100))
        ]
    (border-panel :hgap 10 :vgap 10
      :center (left-right-split
                (grid-panel
                  :hgap 5
                  :rows 1
                  :items [
                    (scrollable l1)
                    (scrollable l2)
                    (scrollable l3)])
                (grid-panel
                  :hgap 5
                  :columns 8
                  :items (into [] (repeat 64 "1"))))
      :north (horizontal-panel
               :border "Supertone"
               :items [(label :text "MENU STUFF HERE?")])
      :south (grid-panel
               :border "DRUM BUS"
               :hgap 5
               :rows 1
               :items [(button :text "Next"
          :mnemonic \N
          :listen [:action #(alert (str %))])]))))

(defn make-frame
  []
  (frame :title "Supertone"
         :content (content "#101010" "#f0f0f0" "MONOSPACED-PLAIN-12")
         :size [800 :by 600]
         :on-close :dispose))

(defn open
  []
  (when @frame* (.dispose @frame*))
  (reset! frame* (make-frame))
  (move-to-center @frame*)
  (move-to-front @frame*))

(defn init
  [s]
  (map->GUI {
    :frame       (util/swap-or frame* (:frame s) (open))
    :controllers (util/swap-or controllers* (:controllers s) [])
    }))

(defn dispose
  [s]
  (map->GUI {
    :frame       (or @frame* (:frame s))
    :controllers (or @controllers* (:controllers s))
    }))
