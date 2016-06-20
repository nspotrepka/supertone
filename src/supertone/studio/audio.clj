(ns supertone.studio.audio
  (:require [overtone.libs.event                :only [event]
                                                :refer :all]
            [overtone.sc.server                 :only [ensure-connected!]
                                                :refer :all]
            [overtone.sc.machinery.synthdef     :only [load-synthdef]
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
            [supertone.studio.bus               :as bus]
            [supertone.studio.control           :as control]))

(def nil-id -1000)

(definst mono-pass
  [in-bus nil-id]
  (in in-bus))

(definst stereo-pass
  [in-bus nil-id]
  (in in-bus 2))

(definst sawboy
  [freq {:default 200 :min 2 :max 20000 :step 1}]
  [(saw freq) (saw freq)])

(defrecord Audio [inst-map inst-ctl inst-nodes fx-map fx-ctl inst-io])

(def inst-map* (atom nil))
(def inst-ctl* (atom nil))
(def inst-nodes* (atom nil))
(def fx-map* (atom nil))
(def fx-ctl* (atom nil))
(def inst-io* (atom nil))
(def node-order* (atom nil))

(defn init
  [s]
  (map->Audio {
    :inst-map   (util/swap-or inst-map* (:inst-map s) {})
    :inst-ctl   (util/swap-or inst-ctl* (:inst-ctl s) {})
    :inst-nodes (util/swap-or inst-nodes* (:inst-nodes s) {})
    :fx-map     (util/swap-or fx-map* (:fx-map s) {})
    :fx-ctl     (util/swap-or fx-ctl* (:fx-ctl s) {})
    :inst-io    (util/swap-or inst-io* (:inst-io s) {})
    :node-order (util/swap-or node-order* (:node-order s) {})}))

(defn dispose
  [s]
  (map->Audio {
    :inst-map   (or @inst-map* (:inst-map s))
    :inst-ctl   (or @inst-ctl* (:inst-ctl s))
    :inst-nodes (or @inst-nodes* (:inst-nodes s))
    :fx-map     (or @fx-map* (:fx-map s))
    :fx-ctl     (or @fx-ctl* (:fx-ctl s))
    :inst-io    (or @inst-io* (:inst-io s))
    :node-order (or @node-order* (:node-order s))}))

(defn inst-library
  "List the instruments available to add."
  []
  (:instruments @studio*))

(defn inst-list
  "List instrument names."
  []
  (or (keys @inst-map*) '()))

(defn inst-get
  "Get instrument by name."
  [name]
  (get @inst-map* name))

(defn inst-ctl-nodes
  "Get the list of synths controlling an instrument parameter."
  [name pname]
  (or (get-in @inst-ctl* [name pname]) []))

(defn inst-nodes
  "Get all the active nodes associated with an instrument."
  [name]
  (or (get @inst-nodes* name) []))

(defn- param-map
  [synth pname]
  (some #(when (= (:name %) pname) %) (:params synth)))

(defn- param-map-synth
  [synth pname]
  (merge
    {:min 0 :max 100000 :step 1}
    (param-map synth pname)))

(defn- param-map-ctl
  [synth pname]
  (merge
    {:min 0 :max 0 :step 0}
    (param-map synth pname)))

(defn inst-params
  "Get instrument parameter names."
  [name]
  (remove
    #{"in-bus"}
    (:args (inst-get name))))

(defn inst-param
  "Get instrument parameter."
  [name pname]
  (let [a         (:value (param-map-synth (inst-get name) pname))
        ctl-nodes (inst-ctl-nodes name pname)]
    (if (empty? ctl-nodes)
      (if (nil? a) nil (float @a))
      (control-bus-get
        (int (node-get-control (first ctl-nodes) :in-bus1))))))

(defn inst-param!
  "Set instrument parameter."
  [name pname value]
  (let [inst      (inst-get name)
        ctl-nodes (inst-ctl-nodes name pname)]
    (swap! (:value (param-map-synth (inst-get name) pname)) (constantly value))
    (if (empty? ctl-nodes)
      (ctl inst pname value)
      (control-bus-set!
        (int (node-get-control (first ctl-nodes) :in-bus1))
        value))))

(defn inst-param-reset!
  [name pname]
  (inst-param!
    name
    pname
    (:default (param-map-synth (inst-get name) pname))))

(defn inst-param-delta!
  "Change instrument parameter based on step size."
  [name pname delta]
  (let [all     (param-map-synth (inst-get name) pname)
        min-val (:min all)
        max-val (:max all)
        step    (:step all)]
    (util/delta
      [name pname]
      inst-param
      inst-param!
      min-val
      max-val
      step
      delta)))

(defn inst-ctl-add!
  "Add a control bus to an instrument parameter."
  [name pname bus]
  (control/mix-add!
    inst-ctl*
    [name pname]
    bus
    (inst-ctl-nodes name pname)
    (inst-nodes name)
    inst-param))

(defn inst-ctl-remove!
  "Remove a control node from an instrument parameter."
  [name pname ctl-node]
  (control/mix-remove!
    inst-ctl*
    [name pname]
    ctl-node
    (inst-ctl-nodes name pname)
    (inst-nodes name)
    inst-param
    inst-param!))

(defn inst-ctl-clear!
  "Clear all control nodes from an instrument parameter."
  [name pname]
  (dorun (map
    #(inst-ctl-remove! name pname %)
    (inst-ctl-nodes name pname))))

(defn ctl-amt-get
  "Get the control's magnitude."
  [ctl-node]
  (let [nodes (if (sequential? ctl-node) ctl-node [ctl-node])]
    (util/single (map
      #(node-get-control % :amt)
      nodes))))

(defn ctl-amt-set
  "Set the control's magnitude."
  [ctl-node amt]
  (let [nodes (if (sequential? ctl-node) ctl-node [ctl-node])
        amts  (if (sequential? amt) amt (repeat (count nodes) amt))]
    (util/single (dorun (map
      #(node-control* %1 [:amt %2])
      nodes amts)))))

(defn ctl-amt-reset
  [ctl-node]
  (ctl-amt-set ctl-node 0.0))

(defn inst-ctl-amt-delta
  "Change the instrument control's magnitude based on step size."
  [name pname ctl-node delta]
  (let [all     (param-map-ctl (inst-get name) pname)
        min-val (:min all)
        max-val (:max all)
        big-val (- max-val min-val)
        step    (:step all)]
    (util/delta
      [ctl-node]
      ctl-amt-get
      ctl-amt-set
      (* big-val -1)
      big-val
      step
      delta)))

(defn inst-in
  "Get instrument input bus."
  [name]
  (inst-param name "in-bus"))

(defn inst-out
  "Get instrument output bus."
  [name]
  (node-get-control (:mixer (inst-get name)) :out-bus))

(defn fx-nodes
  "Get the fx nodes of an instrument"
  [name]
  (or (flatten (map #(:node %) (get @fx-map* name))) []))

(defn fx-get-all
  "Get instrument fx chain."
  [name]
  (get @fx-map* name))

(defn fx-get
  "Get instrument fx synth."
  [name fx-node]
  (some
    #(when
      (or
        (= (:node %) fx-node)
        (contains? (set (:node %)) fx-node))
      %)
    (fx-get-all name)))

(defn fx-ctl-nodes
  "Get the list of synths controlling an fx parameter."
  [name fx-node pname]
  (or (get-in @fx-ctl* [name fx-node pname]) []))

(defn fx-params
  "Get all fx parameters."
  [name fx-node]
  (:args (:synth (fx-get name fx-node))))

(defn fx-param
  "Get fx parameter(s)."
  [name fx-node pname]
  (let [nodes (if (sequential? fx-node) fx-node [fx-node])]
    (util/single (map
      (fn [fx-n]
        (let [ctl-nodes (fx-ctl-nodes name fx-n pname)]
          (if (empty? ctl-nodes)
            (node-get-control fx-n pname)
            (control-bus-get
              (int (node-get-control (first ctl-nodes) :in-bus1))))))
      nodes))))

(defn fx-in
  "Get fx input busses."
  ([name] (reduce #(into %1 (fx-in name %2)) [] (fx-nodes name)))
  ([name fx-node]
   (let [ugens    (:ugens (:sdef (:synth (fx-get name fx-node))))
         ugens-in (filter #(= (:name %) "In") ugens)
         arg-maps (map #(:arg-map %) ugens-in)
         busses   (flatten (map
                    #(map
                      (fn [fx-n]
                        (bus/float-range
                          (node-get-control fx-n (:name (:bus %)))
                          (:num-channels %)))
                      (if (sequential? fx-node) fx-node [fx-node]))
                    arg-maps))
         external (remove (set (bus/bus-range (:bus (inst-get name)))) busses)
         f        (set (map #(float %) external))]
     (into [] (sort f)))))

(defn fx-out
  "Get fx output busses."
  ([name] [])
  ([name fx-node] []))

(defn fx-index
  "Get index of fx."
  [name fx-node]
  (.indexOf (fx-get-all name) (fx-get name fx-node)))

(defn fx-move-before!
  "Move fx before on instrument."
  [name fx-node]
  (let [index  (fx-index name fx-node)
        before (dec index)]
    (when (and (>= index 1) (< index (count (fx-get-all name))))
      (swap! fx-map* update-in [name] util/swap index before)
      (let [other-node (:node ((fx-get-all name) index))
            nodes (if (sequential? fx-node) fx-node [fx-node])]
        (dorun (map
          #(node-place* % :before (first other-node))
          nodes))))))

(defn fx-move-after!
  "Move fx after on instrument."
  [name fx-node]
  (let [index (fx-index name fx-node)
        after (inc index)]
    (when (and (>= index 0) (< index (dec (count (fx-get-all name)))))
      (swap! fx-map* update-in [name] util/swap index after)
      (let [other-node (:node ((fx-get name) index))
            nodes (if (sequential? fx-node) fx-node [fx-node])]
        (dorun (map
          #(node-place* % :after (last other-node))
          (reverse nodes)))))))

(defn inst-io-swap!
  "Update the input/output info for an instrument."
  [name]
  (let [inst    (inst-get name)
        n-chans (:n-chans inst)
        in-vec  (bus/float-range (inst-in name) n-chans)
        out-vec (bus/float-range (inst-out name) n-chans)
        io      {:in in-vec :out out-vec}
        fx-in   (fx-in name)
        fx-out  (fx-out name)
        io-all  (update-in (update-in io [:in] into fx-in) [:out] into fx-out)
        io-now  (get @inst-io* name)]
    (swap! inst-io* assoc name io-all)
    (not= io-now io-all)))

(defn inst-io
  "Get all the input/output busses to/from an instrument."
  []
  @inst-io*)

(defn bus-io
  "Get all the input/output instruments to/from a bus."
  []
  (util/map-invert (inst-io)))

(defn sort-node-tree!
  "Sort the instruments in the node tree."
  []
  (let [io-inst     (inst-io)
        io-bus      (util/map-invert io-inst)
        sinks       (into
                      clojure.lang.PersistentQueue/EMPTY
                      (filter #(empty? (:out (get io-bus %))) (keys io-bus)))
        dummy-group (groups/audio-dummy)]
    (swap! node-order* (constantly (into [] (distinct
      (loop [i-map  io-inst
             b-map  io-bus
             n      (peek sinks)
             queue  (pop sinks)
             sorted '()]
        (let [s      (string? n)
              f      (float? n)
              in-vec (cond
                       s (get-in i-map [n :in])
                       f (get-in b-map [n :in])
                       :else nil)
              i-new  (if s (assoc-in i-map [n :in] nil) i-map)
              b-new  (if f (assoc-in b-map [n :in] nil) b-map)]
          (when s (node-place* (:group (inst-get n)) :after dummy-group))
          (if (or s f)
            (let [q-new (reduce #(conj %1 %2) queue in-vec)]
              (recur i-new b-new (peek q-new) (pop q-new) (conj sorted n)))
            (if n (conj sorted n) sorted))))))))))

(defn node-order
  "Get the order of nodes. Must call sort-node-tree beforehand."
  []
  @node-order*)

(defn inst-in!
  "Set instrument input bus."
  [name bus]
  (let [bus2 (if bus bus nil-id)]
    (when (inst-in name)
      (inst-param! name "in-bus" (float bus2))
      (inst-io-swap! name)
      (sort-node-tree!))))

(defn inst-out!
  "Set instrument output bus."
  [name bus]
  (let [bus2 (if bus bus nil-id)]
    (ctl (:mixer (inst-get name)) :out-bus bus2)
    (inst-io-swap! name)
    (sort-node-tree!)))

(defn fx-param!
  "Set fx parameter(s). Pass values as a vector for multiple channels."
  [name fx-node pname value]
  (let [nodes (if (sequential? fx-node) fx-node [fx-node])
        vals  (if (sequential? value) value (repeat (count nodes) value))]
    (util/single (dorun (map
      (fn [fx-n val]
        (let [ctl-nodes (fx-ctl-nodes name fx-n pname)]
          (if (empty? ctl-nodes)
            (node-control* fx-n [pname val])
            (control-bus-set!
              (int (node-get-control (first ctl-nodes) :in-bus1))
              val))))
      nodes vals))))
    (when (inst-io-swap! name)
      (sort-node-tree!)))

(defn fx-param-reset!
  [name fx-node pname]
  (fx-param!
    name
    fx-node
    pname
    (:default (param-map-synth (:synth (fx-get name fx-node)) pname))))

(defn fx-param-delta!
  "Change instrument parameter based on step size."
  [name fx-node pname delta]
  (let [all     (param-map-synth (:synth (fx-get name fx-node)) pname)
        min-val (:min all)
        max-val (:max all)
        step    (:step all)
        nodes   (if (sequential? fx-node) fx-node [fx-node])]
      (dorun (map
        #(util/delta
          [name % pname]
          fx-param
          fx-param!
          min-val
          max-val
          step
          delta)
        nodes))))

(defn fx-ctl-add!
  "Add a control bus to an fx parameter."
  [name fx-node pname bus]
  (let [nodes (if (sequential? fx-node) fx-node [fx-node])]
    (util/single (doall (map
      (fn [fx-n]
        (control/mix-add!
          fx-ctl*
          [name fx-n pname]
          bus
          (fx-ctl-nodes name fx-n pname)
          [fx-n]
          fx-param))
      nodes)))))

(defn fx-ctl-remove!
  "Remove a control node from an instrument parameter."
  [name fx-n pname ctl-node]
  (let [nodes (if (sequential? ctl-node) ctl-node [ctl-node])]
    (util/single (dorun (map
      (fn [ctl-n]
        (control/mix-remove!
          fx-ctl*
          [name fx-n pname]
          ctl-n
          (fx-ctl-nodes name fx-n pname)
          [fx-n]
          fx-param
          fx-param!))
      nodes)))))

(defn fx-ctl-clear!
  "Clear all control nodes from an instrument parameter."
  [name fx-node pname]
  (dorun (map
    (fn [fx-n]
      (dorun (map
        #(fx-ctl-remove! name fx-n pname %)
        (fx-ctl-nodes name fx-n pname))))
    (if (sequential? fx-node) fx-node [fx-node]))))

(defn fx-ctl-amt-delta
  "Change the fx control's magnitude based on step size."
  [name fx-node pname ctl-node delta]
  (let [all     (param-map-ctl (:synth (fx-get name fx-node)) pname)
        min-val (:min all)
        max-val (:max all)
        big-val (- max-val min-val)
        step    (:step all)
        nodes   (if (sequential? ctl-node) ctl-node [ctl-node])]
    (dorun (map
      #(util/delta
        [%]
        ctl-amt-get
        ctl-amt-set
        (* big-val -1)
        big-val
        step
        delta)
      nodes))))

(defn- sdef-set-bus
  [sdef bus]
  (update
    (update sdef :constants #(conj (take (- (count %) 1) %) bus))
    :ugens
    (fn [u]
      (concat
        (take (- (count u) 1) u)
        [(update
          (update
            (update
              (last u)
              :arg-map
              #(assoc % :bus (int bus)))
            :orig-args
            #(conj (rest %) (int bus)))
          :args
          #(conj (rest %) bus))]))))

(defn inst-add!
  "Duplicate an new instrument.
   Usage: (inst-add! \"mono-pass\" \"example\")"
  [old-name new-name]
  (when (inst-get new-name)
    (throw (Exception. (str "Instument already exists: " new-name))))
  (ensure-connected!)
  (let [old-name# old-name
       new-name# new-name
       old-inst# (get (inst-library) old-name#)
       n-chans#  (:n-chans old-inst#)
       inst-bus# (bus/audio n-chans#)
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

       sdef#      (update
                    (sdef-set-bus (:sdef old-inst#) (bus/bus-id inst-bus#))
                    :name
                    #(str % "-" new-name#))
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
   (load-synthdef sdef#)
   (swap! inst-map* assoc new-name# inst#)
   (swap! inst-ctl* assoc new-name# {})
   (swap! inst-nodes* assoc new-name# [])
   (swap! fx-map* assoc new-name# [])
   (swap! fx-ctl* assoc new-name# {})
   (inst-io-swap! new-name#)
   (sort-node-tree!)
   inst#))

 (defn inst-rename!
   "Rename an instrument."
   [name new-name]
   (when (inst-get new-name)
     (throw (Exception. (str "Instument already exists: " new-name))))
   (let [inst       (inst-get name)
         inst-ctl   (get @inst-ctl* name)
         inst-nodes (get @inst-nodes* name)
         fx         (fx-get-all name)
         fx-ctl     (get @fx-ctl* name)
         io         (get (inst-io) name)
         index      (.indexOf (node-order) name)]
     (swap! inst-map* dissoc name)
     (swap! inst-ctl* dissoc name)
     (swap! inst-nodes* dissoc name)
     (swap! fx-map* dissoc name)
     (swap! fx-ctl* dissoc name)
     (swap! inst-io* dissoc name)
     (swap! inst-map* assoc new-name inst)
     (swap! inst-ctl* assoc new-name inst-ctl)
     (swap! inst-nodes* assoc new-name inst-nodes)
     (swap! fx-map* assoc new-name fx)
     (swap! fx-ctl* assoc new-name fx-ctl)
     (swap! inst-io* assoc new-name io)
     (swap! node-order* #(assoc % index new-name))
     inst))

(defn inst-remove!
  "Remove an instrument."
  [name]
  (let [inst (inst-get name)]
    (dorun (map
      (fn [fx-node]
        (dorun (map
          (partial fx-ctl-clear! name fx-node)
          (fx-params name fx-node))))
      (fx-nodes name)))
    (dorun (map
      (partial inst-ctl-clear! name)
      (inst-params name)))
    (swap! inst-map* dissoc name)
    (swap! inst-ctl* dissoc name)
    (swap! inst-nodes* dissoc name)
    (swap! fx-map* dissoc name)
    (swap! fx-ctl* dissoc name)
    (swap! inst-io* dissoc name)
    (bus/free (:bus inst))
    (node-free* (:group inst))
    nil))

(defn fx-add!
  "Add fx to instrument."
  [name fx]
  (let [inst (inst-get name)
        fx-node (inst-fx! inst fx)]
    (swap! fx-map* update-in [name] conj {:node fx-node :synth fx})
    (when (inst-io-swap! name)
      (sort-node-tree!))
    fx-node))

(defn fx-remove!
  "Remove an instrument's fx."
  [name fx-node]
  (dorun (map
    (partial fx-ctl-clear! name fx-node)
    (fx-params name fx-node)))
  (swap! fx-map* update-in [name] (partial remove #(= (:node %) fx-node)))
  (dorun (map #(node-free* %) (if (sequential? fx-node) fx-node [fx-node])))
  (inst-io-swap! name)
  nil)

(defn inst-add-to-bus!
  "Add an instrument and connect it to input and output busses."
  [old-name new-name in-bus out-bus]
  (let [inst (inst-add! old-name new-name)]
    (inst-in! new-name in-bus)
    (inst-out! new-name out-bus)
    inst))

(defn stereo?
  "Check if an instrument or a bus is stereo."
  [n]
  (cond
    (string? n) (> (:n-chans (inst-get n)) 1)
    (float? n) (bus/alloc-audio-id? n 2)
    :else (throw (Exception. (str "Unknown type: " n)))))

(defn pass-in!
  "Route the input of an instrument or a bus through another pass."
  ([n new-name] (pass-in! n new-name :mono))
  ([n new-name channels]
   (let [stereo (= channels :stereo)
         old-name (if stereo "stereo-pass" "mono-pass")
         inst (inst-add! old-name new-name)
         bus (bus/bus-id (bus/audio (if stereo 2 1)))]
     (cond
       (string? n) (let [in (inst-in n)]
                     (inst-out! new-name bus)
                     (inst-in! new-name in)
                     (inst-in! n bus))
       (float? n) (let [in (filter #(= (inst-out %) (float n)) (inst-list))]
                    (inst-in! new-name bus)
                    (dorun (map #(inst-out! % bus) in))
                    (inst-out! new-name n))
       :else (throw (Exception. (str "Unknown type: " n)))))))

(defn pass-out!
  "Route the output of an instrument or a bus through another pass."
  ([n new-name] (pass-out! n new-name :mono))
  ([n new-name channels]
   (let [stereo (= channels :stereo)
         old-name (if stereo "stereo-pass" "mono-pass")
         inst (inst-add! old-name new-name)
         bus (bus/bus-id (bus/audio (if stereo 2 1)))]
     (cond
       (string? n) (let [out (inst-out n)]
                     (inst-in! new-name bus)
                     (inst-out! new-name out)
                     (inst-out! n bus))
       (float? n) (let [out (filter #(= (inst-in %) (float n)) (inst-list))]
                    (inst-out! new-name bus)
                    (dorun (map #(inst-in! % bus) out))
                    (inst-in! new-name n))
       :else (throw (Exception. (str "Unknown type: " n)))))))

(defn inst-node-add!
  "Add node for an instrument."
  [name]
  (let [inst-node ((inst-get name))]
    (dorun (map
      #(when-let [ctl-nodes (seq (inst-ctl-nodes name %))]
         (node-map-controls*
           inst-node
           [% (int (node-get-control (last ctl-nodes) :out-bus))]))
      (inst-params name)))
    (swap! inst-nodes* update name
      #(into % [inst-node]))
    inst-node))

(defn inst-node-remove!
  "Remove node for an instrument."
  [name inst-node]
  (node-free* inst-node)
  (swap! inst-nodes* update name
    #(into [] (remove #{inst-node} %)))
  nil)

(defn inst-node-clear!
  "Clear all nodes for an instrument."
  [name]
  (dorun (map (partial inst-node-remove! name) (inst-nodes name))))

(defn clear-busses
  "Free all unused busses."
  []
  (dorun (map
    bus/free-audio-id
    (remove
      #(contains? (into #{} (keys (bus-io))) %)
      (drop (:n-channels bus/hardware) @bus/bus-audio*)))))

(defn wipe
  "Clear all audio processing."
  []
  (dorun (map inst-remove! (inst-list)))
  (dorun (map
    bus/free-audio-id
    (drop (:n-channels bus/hardware) @bus/bus-audio*))))
