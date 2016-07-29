(ns supertone.studio.control
  (:require [overtone.sc.bus         :refer :all]
            [overtone.sc.node        :refer :all]
            [overtone.sc.ugens       :refer :all]
            [overtone.sc.synth       :refer :all]
            [supertone.util          :as util]
            [supertone.studio.bus    :as bus]
            [supertone.studio.groups :as groups]))

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

(defrecord Control [store])

(def store* (atom nil))

(defn init
  [s]
  (map->Control {
    :store (util/swap-or store* (:store s) {})}))

(defn dispose
  [s]
  (map->Control {
    :store (or @store* (:store s))}))

;; Not quite adequate, you should rewrite this
(defn library
  "List the control synths available to add."
  []
  (->> (util/list-synth)
    (filter #(= (type %) :overtone.sc.synth/synth))))

(defn control-list
  "List control busses."
  []
  (or (keys @store*) '()))

(defn- control-get
  [bus]
  (get @store* bus))

(defn control-get-node
  "Get the node for a control."
  [bus]
  (:node (control-get bus)))

(defn control-get-synth
  "Get the synth for a control."
  [bus]
  (:synth (control-get bus)))

(defn param-list
  "Get all control parameters."
  [bus]
  (remove
    #{"out-bus"}
    (:args (control-get-synth bus))))

(defn param
  "Get a control parameter."
  [bus pname]
  (node-get-control (control-get-node bus) pname))

(defn param!
  "Set a control parameter."
  [bus pname value]
  (ctl (control-get-node bus) pname value))

(defn add!
  "Add a control. Control synth must have out-bus param."
  [s & args]
  (let [bus (bus/control)
        n   (apply (partial s [:tail (groups/control)]) args)]
    (node-control* n [:out-bus (:id bus)])
    (swap! store* assoc bus {:node n :synth s})
    bus))

(defn remove!
  "Remove a control."
  [bus]
  (node-free* (control-get-node bus))
  (bus/free bus)
  (swap! store* dissoc bus)
  nil)

(defn- map-control-bus-to-nodes
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
    (map-control-bus-to-nodes audio-nodes (last id) (bus/bus-id out-bus))
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
        (map-control-bus-to-nodes audio-nodes (last id) in-bus1))
      (swap! ctl-atom update-in id
        #(into [] (remove #{ctl-node} %)))
      (when (= (count ctl-nodes) 1)
        (apply param-set-fn (into id [(control-bus-get (int in-bus1))]))
        (bus/free-control-id in-bus1))
      (node-free* ctl-node)
      (bus/free-control-id out-bus))
    nil))
