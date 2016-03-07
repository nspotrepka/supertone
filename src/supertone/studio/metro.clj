(ns supertone.studio.metro
  (:require [overtone.libs.event     :refer :all]
            [overtone.sc.bus         :refer :all]
            [overtone.sc.ugens       :refer :all]
            [overtone.sc.node        :refer :all]
            [overtone.sc.synth       :refer :all]
            [overtone.sc.trig        :refer :all]
            [supertone.util          :as util]
            [supertone.studio.groups :as groups]))

(defrecord Metro [ppq bpm metro-node metro-count])

(def metro-id 0)

(def ppq* (atom nil))
(def bpm* (atom nil))
(def metro-node* (atom nil))
(def metro-count* (atom nil))

(defn init
  [s]
  (map->Metro {
    :ppq          (util/swap-or ppq* (:ppq s)
                    (let [b (control-bus)] (control-bus-set! b 96) b))
    :bpm          (util/swap-or bpm* (:bpm s)
                    (let [b (control-bus)] (control-bus-set! b 120) b))
    :metro-node   (util/swap-or metro-node* (:metro-node s))
    :metro-count  (util/swap-or metro-count* (:metro-count s) 0)}))

(defn dispose
  [s]
  (map->Metro {
    :ppq         (or @ppq* (:ppq s))
    :bpm         (or @bpm* (:bpm s))
    :metro-node  (or @metro-node* (:metro-node s))
    :metro-count (or @metro-count* (:metro-count s))}))

(defsynth metro-synth
  "Simple metronome."
  [bpm-bus 0 ppq-bus 0]
  (let [trigger (impulse:ar (* (in:kr ppq-bus) (/ (in:kr bpm-bus) 60)))
        count (pulse-count:ar trigger 0)]
    (send-trig:ar trigger metro-id (- count 1))))

(defn metro-play
  "Start the metronome. Create a new metronome if one does not exist."
  []
  (swap! metro-node*
    (fn [s] (if s (node-start s)
                  (metro-synth @bpm* @ppq* :tail (groups/metro))))))

(defn metro-pause
  "Pause the metronome. Will restart from the same spot when played."
  []
  (swap! metro-node* (fn [s] (when s (node-pause* s)))))

(defn metro-reset
  "Reset the metronome."
  []
  (swap! metro-node* (fn [s] (when s (kill s))))
  (swap! metro-count* (constantly 0)))

(defn on-metro
  "Adds a handler for the metronome."
  [handler key]
  (on-trigger metro-id handler key))

(defn off-metro
  "Adds a handler for the metronome."
  [key]
  (remove-event-handler key))

;; TEST CODE

(on-metro println ::println)
(remove-event-handler ::println)
