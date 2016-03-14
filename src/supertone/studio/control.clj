(ns supertone.studio.control
  (:require [overtone.libs.event     :only [event]
                                     :refer :all]
            [overtone.sc.node        :refer :all]
            [overtone.sc.ugens       :refer :all]
            [overtone.sc.synth       :refer :all]
            [supertone.util          :as util]
            [supertone.studio.groups :as groups]
            [supertone.studio.bus    :as bus]))

;; CONTROL SYNTHS GO HERE

(defrecord Control [ctl-map ctl-io])

(def ctl-map* (atom nil))
(def ctl-io* (atom nil))

(defn init
  [s]
  (map->Control {
    :ctl-map (util/swap-or ctl-map* (:ctl-map s) {})
    :ctl-io  (util/swap-or ctl-io* (:ctl-io s) {})
    }))

(defn dispose
  [s]
  (map->Control {
    :ctl-map (or @ctl-map* (:ctl-map s))
    :ctl-io  (or @ctl-io* (:ctl-io s))
    }))

(defn ctl-param
  "Get a control parameter."
  [n pname]
  (node-get-control n pname))

(defn ctl-param!
  "Set a control parameter"
  [n pname value]
  (ctl n pname value))

(defn ctl-in
  "Get control node input bus."
  [n]
  (ctl-param n "in-bus"))

(defn ctl-out
  "Get control node output bus."
  [n]
  (ctl-param n "out-bus"))

(defn ctl-io-swap!
  "Update the input/output info for a control node."
  [n]
  (let [in-vec  [(ctl-in n)]
        out-vec [(ctl-out n)]
        io-all  {:in in-vec :out out-vec}]
    (swap! ctl-io* assoc n io-all)
    io-all))

(defn ctl-io
  "Get all the input/output busses to/from a control node."
  []
  @ctl-io*)

(defn bus-io
  "Get all the input/output control nodes to/from a bus."
  []
  (util/map-invert (ctl-io)))

(defn sort-node-tree
  "Sort the control nodes in the node tree."
  []
  (let [io-ctl      (ctl-io)
        io-bus      (util/map-invert io-ctl)
        sinks       (into
                      clojure.lang.PersistentQueue/EMPTY
                      (filter #(nil? (:out (get io-bus %))) (keys io-bus)))
        dummy-group (groups/control-dummy)]
    (loop [c-map  io-ctl
           b-map  io-bus
           n      (peek sinks)
           queue  (pop sinks)]
      (let [s      (node? n)
            f      (float? n)
            in-vec (cond
                     s (get-in c-map [n :in])
                     f (get-in b-map [n :in])
                     :else nil)
            i-new  (if s (assoc-in c-map [n :in] nil) c-map)
            b-new  (if f (assoc-in b-map [n :in] nil) b-map)]
        (when s (node-place* n :after dummy-group))
        (when (or s f)
          (let [q-new (reduce #(conj %1 %2) queue in-vec)]
            (recur i-new b-new (peek q-new) (pop q-new))))))))

(defn ctl-in!
  "Set control node input bus."
  [n bus]
  (ctl-param! n :in-bus (float bus))
  (ctl-io-swap! n)
  (sort-node-tree))

(defn ctl-out!
  "Set control node output bus."
  [n bus]
  (ctl-param! n :out-bus (float bus))
  (ctl-io-swap! n)
  (sort-node-tree))

(defn ctl-add!
  "Add a control node."
  [s & args]
  (let [n (apply s args)]
    (event :ctl-add :ctl n)
    (swap! ctl-map* assoc n s)
    (ctl-io-swap! n)
    (sort-node-tree)
    n))

(defn ctl-remove!
  "Remove a control node."
  [n]
  (kill (to-sc-id n))
  (swap! ctl-map* dissoc n)
  (swap! ctl-io* dissoc n)
  nil)
