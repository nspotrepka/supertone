(ns supertone.view.gui
  (:require [seesaw.core    :refer :all]
            [com.evocomputing.colors :refer :all]
            [supertone.util :as util]
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

(defn open
  []
  (when @frame* (.dispose @frame*))
  (reset! frame*
    (frame :title "Hello",
           :content (top-bottom-split
                        (scrollable
                          (listbox
                            :model
                            (-> 'seesaw.core ns-publics keys sort)))
                        (slider)),
           :size [800 :by 600],
           :on-close :dispose))
  (move-to-center @frame*)
  (move-to-front @frame*))

(defn init
  [s]
  (map->GUI {
    :frame       (util/swap-or frame* (:frame s) (println "OPEN GUI"))
    :controllers (util/swap-or controllers* (:controllers s) [])
    }))

(defn dispose
  [s]
  (map->GUI {
    :frame       (or @frame* (:frame s))
    :controllers (or @controllers* (:controllers s))
    }))
