(ns supertone.studio.control
  (:require [overtone.libs.event     :only [event]
                                     :refer :all]
            [overtone.sc.node        :refer :all]
            [overtone.sc.ugens       :refer :all]
            [overtone.sc.synth       :refer :all]
            [supertone.util          :as util]
            [supertone.studio.groups :as groups]
            [supertone.studio.bus    :as bus]))

(def nil-id -1000)

(defsynth mix
  [in-bus1 nil-id
   in-bus2 nil-id
   out-bus nil-id
   amt     0]
  (out:kr out-bus (+ (in:kr in-bus1) (* (in:kr in-bus2) amt))))

(defrecord Control [ctl-map])

(def ctl-map* (atom nil))
(def node-order* (atom nil))

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
  (->> *ns*
    (ns-map)
    (vals)
    (filter #(= (type %) :overtone.sc.synth/synth))
    (sort-by str)))

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

(defn bus-io
  "Get all the input/output control nodes to/from a bus."
  []
  @ctl-map*)

(defn ctl-io
  "Get all the input/output busses to/from a control node."
  []
  (util/map-invert (bus-io)))

(defn sort-node-tree
  "Sort the control nodes in the node tree."
  []
  (let [io-bus      (bus-io)
        io-ctl      (util/map-invert io-bus)
        sinks       (into
                      clojure.lang.PersistentQueue/EMPTY
                      (filter #(nil? (:out (get io-bus %))) (keys io-bus)))
        dummy-group (groups/control-dummy)]
    (swap! node-order* (constantly (into [] (distinct
      (loop [c-map  io-ctl
             b-map  io-bus
             n      (peek sinks)
             queue  (pop sinks)
             sorted '()]
        (let [s      (node? n)
              f      (float? n)
              in-vec (cond
                       s (get-in c-map [n :in])
                       f (get-in b-map [n :in])
                       :else nil)
              i-new  (if s (assoc-in c-map [n :in] nil) c-map)
              b-new  (if f (assoc-in b-map [n :in] nil) b-map)]
          (if (or s f)
            (let [q-new (reduce #(conj %1 %2) queue in-vec)]
              (recur i-new b-new (peek q-new) (pop q-new) (conj sorted n)))
            (if n (conj sorted n) sorted))))))))))

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
