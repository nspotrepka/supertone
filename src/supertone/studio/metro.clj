(ns supertone.studio.metro
  (:require [overtone.sc.bus         :refer :all]
            [overtone.sc.ugens       :refer :all]
            [overtone.sc.node        :refer :all]
            [overtone.sc.synth       :refer :all]
            [overtone.sc.trig        :refer :all]
            [supertone.util          :as util]
            [supertone.studio.bus    :as bus]
            [supertone.studio.groups :as groups]))

(defrecord Metro [ppq bpm metro-node metro-count])

(def metro-id 0)

(def bpm* (atom nil))
(def ppq* (atom nil))
(def metro-node* (atom nil))
(def metro-count* (atom nil))

(defn init
  [s]
  (map->Metro {
    :bpm          (util/swap-or bpm* (:bpm s)
                    (let [b (bus/control 1 "Tempo")]
                      (control-bus-set! b 120)
                      b))
    :ppq          (util/swap-or ppq* (:ppq s)
                    (let [b (bus/control 1 "Beat subdivision")]
                      (control-bus-set! b 96)
                      b))
    :metro-node   (util/swap-or metro-node* (:metro-node s))
    :metro-count  (util/swap-or metro-count* (:metro-count s) 0)}))

(defn dispose
  [s]
  (map->Metro {
    :bpm         (or @bpm* (:bpm s))
    :ppq         (or @ppq* (:ppq s))
    :metro-node  (or @metro-node* (:metro-node s))
    :metro-count (or @metro-count* (:metro-count s))}))

(defsynth metro-synth
  "Simple metronome."
  [bpm-bus 0 ppq-bus 0]
  (let [trigger (impulse:ar (* (in:kr ppq-bus) (/ (in:kr bpm-bus) 60)))
        count (pulse-count:ar trigger 0)]
    (send-trig:ar trigger metro-id (- count 1))))

(defn bpm "Get beats per minute." [] (control-bus-get @bpm*))
(defn bpm! "Set beats per minute." [x] (control-bus-set! @bpm* x))
(defn ppq "Get pulses per beat." [] (control-bus-get @ppq*))
(defn ppq! "Set pulses per beat." [x] (control-bus-set! @ppq* x))

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

(defn add-handler
  "Adds a handler to the metronome."
  [handler key]
  (on-trigger metro-id handler key))

;; TEST CODE

;; (add-handler println ::println)
;; (remove-event-handler ::println)
