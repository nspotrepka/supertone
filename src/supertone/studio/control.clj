(ns supertone.studio.control
  (:require [overtone.libs.event     :only [event]
                                     :refer :all]
            [overtone.sc.bus         :refer :all]
            [overtone.sc.node        :refer :all]
            [overtone.sc.ugens       :refer :all]
            [overtone.sc.synth       :refer :all]
            [supertone.util          :as util]
            [supertone.studio.groups :as groups]
            [supertone.studio.bus    :as bus]))

(def nil-id -2000)

(defsynth mix
  [in-bus1 nil-id
   in-bus2 nil-id
   out-bus nil-id
   amt     0]
  (out:kr
    out-bus
    (+
      (in:kr in-bus1)
      (* (in:kr in-bus2) amt))))

(defrecord Control [ctl-map])

(def ctl-map* (atom nil))

(defn init
  [s]
  (map->Control {
    :ctl-map (util/swap-or ctl-map* (:ctl-map s) {})}))

(defn dispose
  [s]
  (map->Control {
    :ctl-map (or @ctl-map* (:ctl-map s))}))

(defn ctl-library
  "List the control synths available to add."
  []
  (->> (util/list-synth)
    (filter #(= (type %) :overtone.sc.synth/synth))))

(defn ctl-list
  "List control busses."
  []
  (or (keys @ctl-map*) '()))

(defn ctl-get
  "Get control by bus number."
  [bus]
  (get @ctl-map* bus))

(defn ctl-param
  "Get a control parameter."
  [bus pname]
  (node-get-control (ctl-get bus) pname))

(defn ctl-param!
  "Set a control parameter."
  [bus pname value]
  (ctl (ctl-get bus) pname value))

(defn ctl-add!
  "Add a control. Control synth must have out-bus param."
  [s & args]
  (let [bus (bus/control)
        n   (apply (partial s [:tail (groups/control)]) args)]
    (node-control* n [:out-bus (:id bus)])
    (swap! ctl-map* assoc bus n)
    bus))

(defn ctl-remove!
  "Remove a control."
  [bus]
  (node-free* (ctl-get bus))
  (bus/free bus)
  (swap! ctl-map* dissoc bus)
  nil)

(defn node-map-ctl
  "Map a control bus to the parameters of a collection of nodes."
  [nodes param bus]
  (dorun (map
    #(node-map-controls* % [param (int bus)])
    nodes)))

(defn mix-add!
  "Add a control bus to an instrument parameter."
  [ctl-atom id bus ctl-nodes audio-nodes param-get-fn]
  (let [in-bus1  (if (empty? ctl-nodes)
                   (let [b (bus/control)]
                     (control-bus-set! b (apply param-get-fn id))
                     b)
                   (node-get-control (last ctl-nodes) :out-bus))
        out-bus  (bus/control)
        ctl-node (mix
                   [:tail (groups/control)]
                   :in-bus1 in-bus1
                   :in-bus2 bus
                   :out-bus out-bus)]
    (node-map-ctl audio-nodes (last id) (bus/bus-id out-bus))
    (swap! ctl-atom update-in id
      #(into (if (nil? %) [] %) [ctl-node]))
    ctl-node))

(defn mix-remove!
  "Remove a control node from an instrument parameter."
  [ctl-atom id ctl-node ctl-nodes audio-nodes param-get-fn param-set-fn]
  (let [in-bus1 (node-get-control ctl-node "in-bus1")
        out-bus (node-get-control ctl-node "out-bus")
        index   (.indexOf ctl-nodes ctl-node)]
    (when (>= index 0)
      (if (< (+ index 1) (count ctl-nodes))
        (let [next-node (get ctl-nodes (+ index 1))]
          (node-control* next-node ["in-bus1" (int in-bus1)]))
        (node-map-ctl audio-nodes (last id) in-bus1))
      (swap! ctl-atom update-in id
        #(into [] (remove #{ctl-node} %)))
      (when (= (count ctl-nodes) 1)
        (apply param-set-fn (into id [(control-bus-get (int in-bus1))]))
        (bus/free-control-id in-bus1))
      (node-free* ctl-node)
      (bus/free-control-id out-bus))
    nil))
