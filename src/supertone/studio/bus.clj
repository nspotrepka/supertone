(ns supertone.studio.bus
  (:require [overtone.sc.bus  :refer :all]
            [overtone.sc.info :refer :all]
            [supertone.util   :as util]))

(def hardware (overtone.sc.bus.AudioBus.
                0
                (+ (server-num-input-buses) (server-num-output-buses))
                :audio
                "Reserved Audio Busses"))

(defrecord Bus [bus-audio bus-control])

(def bus-audio* (atom nil))
(def bus-control* (atom nil))

(defn float-range
  "Create a range of bus numbers."
  [index n-chans]
  (if (or (nil? index) (< index 0))
    []
    (into [] (map float (take n-chans (drop index (range)))))))

(defn bus-range
  "Convert a bus to a vector of bus numbers."
  [bus]
  (float-range (:id bus) (:n-channels bus)))

(defn bus-into
  "Add bus indices to a vector."
  [vec bus]
  (sort (into vec (bus-range bus))))

(defn init
  [s]
  (map->Bus {
    :bus-audio   (util/swap-or bus-audio*
                                   (:bus-audio s)
                                   (bus-into [] hardware))
    :bus-control (util/swap-or bus-control*
                                   (:bus-control s)
                                   '())
    }))

(defn dispose
  [s]
  (map->Bus {
    :bus-audio   (or @bus-audio* (:bus-audio s))
    :bus-control (or @bus-control* (:bus-control s))
    }))

(defn bus-id
  "Get the id of a bus."
  [bus]
  (float (:id bus)))

(defn bus-audio
  "Get the list of audio busses."
  []
  @bus-audio*)

(defn bus-control
  "Get the list of control busses."
  []
  @bus-control*)

(defn audio
  "Allocate an audio bus."
  ([] (audio 1 ""))
  ([n] (audio n ""))
  ([n name]
   (let [bus (audio-bus n name)]
     (swap! bus-audio* bus-into bus)
     bus)))

(defn control
  "Allocate a control bus."
  ([] (control 1 ""))
  ([n] (control n ""))
  ([n name]
   (let [bus (control-bus n name)]
     (swap! bus-control* bus-into bus)
     bus)))

(defn alloc-audio-id?
  ([index] (alloc-audio-id? index 1))
  ([index n-chans]
    (=
      (float-range index n-chans)
      (take n-chans (drop-while #(not= (float index) %) @bus-audio*)))))

(defn alloc-control-id?
  ([index] (alloc-control-id? index 1))
  ([index n-chans]
    (=
      (float-range index n-chans)
      (take n-chans (drop-while #(not= (float index) %) @bus-control*)))))

(defn free
  "Free a bus."
  [bus]
  (swap! bus-control* (partial remove (into #{} (bus-range bus))))
  (free-bus bus))

(defn free-audio-id
  "Free an audio bus by id."
  ([index] (free-audio-id index 1))
  ([index n-chans]
   (swap! bus-audio* (partial remove (into #{} (float-range index n-chans))))
   (overtone.sc.machinery.allocator/free-id :audio-bus (int index) n-chans)))

(defn free-control-id
  "Free a control bus by id."
  ([index] (free-control-id index 1))
  ([index n-chans]
   (swap! bus-control* (partial remove (into #{} (float-range index n-chans))))
   (overtone.sc.machinery.allocator/free-id :control-bus (int index) n-chans)))

(defn monitors
  "Returns the bus monitors of a bus."
  [bus]
  (assert bus? bus)
  (map (partial bus-monitor (:id bus)) (range (:n-channels bus))))
