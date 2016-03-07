(ns supertone.studio.audio
  (:require [overtone.libs.event                :only [event]
                                                :refer :all]
            [overtone.sc.server                 :only [ensure-connected!]
                                                :refer :all]
            [overtone.sc.machinery.server.comms :only [with-server-sync]
                                                :refer :all]
            [overtone.sc.bus                    :refer :all]
            [overtone.sc.node                   :refer :all]
            [overtone.sc.ugens                  :refer :all]
            [overtone.sc.synth                  :refer :all]
            [overtone.studio.core               :refer :all]
            [overtone.studio.mixer              :refer :all]
            [overtone.studio.inst               :refer :all]
            [supertone.util                     :as util]
            [supertone.studio.groups            :as groups]
            [supertone.studio.bus               :as bus]))

(definst mono-pass
  [in-bus 10]
  (in in-bus))

(definst stereo-pass
  [in-bus 10]
  (in in-bus 2))

(defrecord Audio [inst-map fx-map inst-io])

(def inst-map* (atom nil))
(def fx-map* (atom nil))
(def inst-io* (atom nil))

(defn init
  [s]
  (map->Audio {
    :inst-map (util/swap-or inst-map* (:inst-map s) {})
    :fx-map   (util/swap-or fx-map* (:fx-map s) {})
    :inst-io  (util/swap-or inst-io* (:inst-io s) {})
    }))

(defn dispose
  [s]
  (map->Audio {
    :inst-map (or @inst-map* (:inst-map s))
    :fx-map   (or @fx-map* (:fx-map s))
    :inst-io  (or @inst-io* (:inst-io s))
    }))

(defn inst-get
  "Get instrument by name."
  [name]
  (get @inst-map* name))

(defn- inst-param-map
  [name pname]
  (first (filter #(= (:name %) pname) (:params (inst-get name)))))

(defn inst-param
  "Get instrument param."
  [name pname]
  (let [a (:value (inst-param-map name pname))]
    (if (nil? a) nil @a)))

(defn inst-param!
  "Set instrument param."
  [name pname value]
  (swap! (:value (inst-param-map name pname)) (constantly value))
  (ctl (inst-get name) pname value))

(defn inst-in
  "Get instrument input bus."
  [name]
  (inst-param name "in-bus"))

(defn inst-out
  "Get instrument output bus."
  [name]
  (node-get-control (:mixer (inst-get name)) :out-bus))

(defn fx-get
  "Get instrument fx chain."
  ([name] (get @fx-map* name))
  ([name fx-node] (first (filter #(= (:node %) fx-node) (fx-get name)))))

(defn fx-in
  "Get fx input busses."
  ([name] (reduce #(into %1 (fx-in name (:node %2))) [] (fx-get name)))
  ([name fx-node]
   (let [ugens    (:ugens (:sdef (:synth (fx-get name fx-node))))
         ugens-in (filter #(= (:name %) "In") ugens)
         arg-maps (map #(:arg-map %) ugens-in)
         busses   (flatten (map
                    #(bus/float-range
                      (node-get-control
                        (util/first-of fx-node)
                        (:name (:bus %)))
                      (:num-channels %))
                    arg-maps))
         external (remove (set (bus/bus-range (:bus (inst-get name)))) busses)
         f        (map #(float %) external)]
     (into [] f))))

(defn fx-out
  "Get fx output busses."
  ([name] [])
  ([name fx-node] []))

(defn fx-index
  "Get index of fx."
  [name fx-node]
  (.indexOf (fx-get name) (fx-get name fx-node)))

(defn fx-param
  "Get fx parameter(s)."
  [name fx-node pname]
  (if (vector? fx-node)
    (into [] (map #(node-get-control % pname) fx-node))
    (node-get-control fx-node pname)))

(defn fx-move-before!
  "Move fx before on instrument."
  [name fx-node]
  (let [inst   (inst-get name)
        index  (fx-index name fx-node)
        before (dec index)]
    (when (and (>= index 1) (< index (count (fx-get name))))
      (swap! fx-map* update-in [name] util/swap index before)
      (let [other-node (:node ((fx-get name) index))]
        (if (vector? fx-node)
          (dorun (map
                   #(node-place* % :before (first other-node))
                   fx-node))
          (node-place* fx-node :before other-node))))))

(defn fx-move-after!
  "Move fx after on instrument."
  [name fx-node]
  (let [inst  (inst-get name)
        index (fx-index name fx-node)
        after (inc index)]
    (when (and (>= index 0) (< index (dec (count (fx-get name)))))
      (swap! fx-map* update-in [name] util/swap index after)
      (let [other-node (:node ((fx-get name) index))]
        (if (vector? fx-node)
          (dorun (map
                   #(node-place* % :after (last other-node))
                   (reverse fx-node)))
          (node-place* fx-node :after other-node))))))

(defn inst-io-swap!
  "Update the input/output info for an instrument."
  [name]
  (let [inst    (inst-get name)
        n-chans (:n-chans inst)
        in      (inst-in name)
        in-vec  (if (nil? in) nil (bus/float-range in n-chans))
        out-vec (bus/float-range (inst-out name) n-chans)
        io      {:in in-vec :out out-vec}
        fx-in   (fx-in name)
        fx-out  (fx-out name)
        io-all  (update-in (update-in io [:in] into fx-in) [:out] into fx-out)]
    (swap! inst-io* assoc name io-all)
    io-all))

(defn inst-io
  "Get all the input/output busses to/from an instrument."
  []
  @inst-io*)

(defn bus-io
  "Get all the input/output instruments to/from a bus."
  []
  (util/map-invert (inst-io)))

(defn sort-node-tree
  "Sort the instruments in the node tree."
  []
  (let [io-inst     (inst-io)
        io-bus      (util/map-invert io-inst)
        sinks       (into
                      clojure.lang.PersistentQueue/EMPTY
                      (filter #(nil? (:out (get io-bus %))) (keys io-bus)))
        dummy-group (groups/audio-dummy)]
    (loop [i-map  io-inst
           b-map  io-bus
           n      (peek sinks)
           queue  (pop sinks)]
      (let [s      (string? n)
            f      (float? n)
            in-vec (cond
                     s (get-in i-map [n :in])
                     f (get-in b-map [n :in])
                     :else nil)
            i-new  (if s (assoc-in i-map [n :in] nil) i-map)
            b-new  (if f (assoc-in b-map [n :in] nil) b-map)]
        (when s (node-place* (:group (inst-get n)) :after dummy-group))
        (when (or s f)
          (let [q-new (reduce #(conj %1 %2) queue in-vec)]
            (recur i-new b-new (peek q-new) (pop q-new))))))))

(defn inst-in!
  "Set instrument input bus."
  [name bus]
  (inst-param! name "in-bus" bus)
  (inst-io-swap! name)
  (sort-node-tree))

(defn inst-out!
  "Set instrument output bus."
  [name bus]
  (ctl (:mixer (inst-get name)) :out-bus bus)
  (inst-io-swap! name)
  (sort-node-tree))

(defmacro inst-add!
  "Duplicate an new instrument.
   Usage: (inst-add! mono-pass \"example\")"
  [old-name new-name]
  (ensure-connected!)
  `(let [old-name# ~old-name
         new-name# ~new-name
         old-inst# (get (:instruments @studio*) old-name#)
         n-chans#  (:n-chans old-inst#)
         inst-bus# (audio-bus n-chans#)
         container-group# (with-server-sync
                            #(group (str "Inst " new-name# " Container")
                                    :tail (:instrument-group @studio*))
                            "whilst creating an inst container group")

         instance-group#  (with-server-sync
                            #(group (str "Inst " new-name#)
                                    :head container-group#)
                            "whilst creating an inst instance group")

         fx-group#  (with-server-sync
                      #(group (str "Inst " new-name# " FX")
                              :tail container-group#)
                      "whilst creating an inst fx group")

         imixer#    (inst-mixer n-chans#
                                [:tail container-group#]
                                :in-bus inst-bus#)

         sdef#      (:sdef old-inst#)
         arg-names# (:args old-inst#)
         params#    (map #(assoc % :value (atom (:default %)))
                         (:params old-inst#))
         fx-chain#  []
         volume#    (atom DEFAULT-VOLUME)
         pan#       (atom DEFAULT-PAN)
         inst#      (with-meta
                      (overtone.studio.inst.Inst. new-name# params# arg-names#
                             sdef# container-group# instance-group# fx-group#
                             imixer# inst-bus# fx-chain#
                             volume# pan# n-chans#)
                      {:overtone.helpers.lib/to-string #(str (name (:type %))
                                                             ":"
                                                             (:name %))})]
     (event :inst-add :inst inst#)
     (swap! inst-map* assoc new-name# inst#)
     (swap! fx-map* assoc new-name# [])
     (inst-io-swap! new-name#)
     (sort-node-tree)
     inst#))

(defn inst-remove!
  "Remove an instrument."
  [name]
  (kill (to-sc-id (:group (inst-get name))))
  (swap! inst-map* dissoc name)
  (swap! fx-map* dissoc name)
  (swap! inst-io* dissoc name)
  nil)

(defn fx-add!
  "Add fx to instrument."
  [name fx]
  (let [inst (inst-get name)
        fx-node (inst-fx! inst fx)]
    (swap! fx-map* update-in [name] conj {:node fx-node :synth fx})
    (inst-io-swap! name)
    (sort-node-tree)
    fx-node))

(defn fx-remove!
  "Remove an instrument's fx."
  [name fx-node]
  (swap! fx-map* update-in [name] (partial remove #(= (:node %) fx-node)))
  (if (vector? fx-node) (dorun (map #(kill %) fx-node)) (kill fx-node))
  (inst-io-swap! name)
  (sort-node-tree))

(defn fx-param!
  "Set fx parameter(s). Pass values as a vector for multiple channels."
  [name fx-node pname values]
  (if (vector? fx-node)
    (dorun (map #(ctl %1 pname %2) fx-node values))
    (ctl fx-node pname values))
  (inst-io-swap! name)
  (sort-node-tree))

;; write higher level functions
