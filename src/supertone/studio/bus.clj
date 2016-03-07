(ns supertone.studio.bus
  (:require [overtone.sc.bus  :refer :all]
            [overtone.sc.info :refer :all]
            [supertone.util   :as util]))

(def hardware (overtone.sc.bus.AudioBus.
                0
                (+ (server-num-input-buses) (server-num-output-buses))
                :audio
                "Reserved Audio Busses"))

(defrecord Bus [bus-vec-audio bus-vec-control])

(def bus-vec-audio* (atom nil))
(def bus-vec-control* (atom nil))

(defn index
  "Get the index of a bus."
  [bus]
  (float (:id bus)))

(defn float-range
  "Create a range of bus numbers."
  [index n-chans]
  (into [] (map float (take n-chans (drop index (range))))))

(defn bus-range
  "Convert a bus to a vector of bus numbers."
  [bus]
  (float-range (:id bus) (:n-channels bus)))

(defn bus-vec-init
  "Append bus indices to a vector."
  [vec bus]
  (into vec (bus-range bus)))

(defn init
  [s]
  (map->Bus {
    :bus-vec-audio   (util/swap-or bus-vec-audio*
                                   (:bus-vec-audio s)
                                   (bus-vec-init [] hardware))
    :bus-vec-control (util/swap-or bus-vec-control*
                                   (:bus-vec-control s)
                                   [])
    }))

(defn dispose
  [s]
  (map->Bus {
    :bus-vec-audio   (or @bus-vec-audio* (:bus-vec-audio s))
    :bus-vec-control (or @bus-vec-control* (:bus-vec-control s))
    }))

(defn audio
  "Allocate an audio bus."
  ([] (let [bus (audio-bus)]
        (swap! bus-vec-audio* bus-vec-init bus)
        bus))
  ([n] (let [bus (audio-bus n)]
         (swap! bus-vec-audio* bus-vec-init bus)
         bus)))

(defn control
  "Allocate a control bus."
  ([] (let [bus (control-bus)]
        (swap! bus-vec-control* bus-vec-init bus)
        bus))
  ([n] (let [bus (control-bus n)]
         (swap! bus-vec-control* bus-vec-init bus)
         bus)))

(defn monitors
  "Returns the bus monitors of a bus."
  [bus]
  (assert bus? bus)
  (map (partial bus-monitor (:id bus)) (range (:n-channels bus))))
