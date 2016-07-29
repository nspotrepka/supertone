(ns supertone.studio.fx
  (:require [overtone.sc.bus                    :refer :all]
            [overtone.sc.node                   :refer :all]
            [overtone.studio.inst               :refer :all]
            [supertone.util                     :as util]
            [supertone.studio.bus               :as bus]
            [supertone.studio.audio             :as audio]
            [supertone.studio.control           :as control]))

(defn control-nodes
  "Get a vector of synths controlling an fx parameter."
  [name fx-node pname]
  (or (get-in @audio/fx-control* [name fx-node pname]) []))

(defn in-busses
  "Get fx input busses."
  ([name] (reduce #(into %1 (in-busses name %2)) [] (audio/fx-list name)))
  ([name fx-node]
   (let [ugens    (:ugens (:sdef (audio/fx-get name fx-node)))
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
         external (remove
                    (set (bus/bus-range (:bus (audio/inst-get name))))
                    busses)
         f        (set (map #(float %) external))]
     (into [] (sort f)))))

(defn out-busses
  "Get fx output busses."
  ([name] [])
  ([name fx-node] []))

(defn param-list
  "Get fx parameter names."
  [name fx-node]
  (:args (audio/fx-get name fx-node)))

(defn param
  "Get fx parameter(s). Returns a vector for multiple channels."
  [name fx-node pname]
  (let [nodes (if (sequential? fx-node) fx-node [fx-node])]
    (util/single (map
      (fn [fx-n]
        (let [ctl-nodes (control-nodes name fx-n pname)]
          (if (empty? ctl-nodes)
            (node-get-control fx-n pname)
            (control-bus-get
              (int (node-get-control (first ctl-nodes) :in-bus1))))))
      nodes))))

(defn param!
  "Set fx parameter(s). Pass in values as a vector for multiple channels."
  [name fx-node pname value]
  (let [nodes    (if (sequential? fx-node) fx-node [fx-node])
        vals     (if (sequential? value) value (repeat (count nodes) value))
        io-other (-> (audio/inst-io name)
                   (audio/inst-io-remove :in (in-busses name fx-node))
                   (audio/inst-io-remove :out (out-busses name fx-node)))]
    (util/single (dorun (map
      (fn [fx-n val]
        (let [ctl-nodes (control-nodes name fx-n pname)]
          (if (empty? ctl-nodes)
            (node-control* fx-n [pname val])
            (control-bus-set!
              (int (node-get-control (first ctl-nodes) :in-bus1))
              val))))
      nodes vals)))
    (when
      (audio/inst-io-swap!
        name
        (-> io-other
          (audio/inst-io-into :in (in-busses name fx-node))
          (audio/inst-io-into :out (out-busses name fx-node))))
      (audio/sort-node-tree!))))

(defn param-reset!
  "Reset fx parameter(s) to default value."
  [name fx-node pname]
  (param!
    name
    fx-node
    pname
    (:default
      (audio/param-map-synth (audio/fx-get name fx-node) pname))))

(defn param-delta!
  "Change fx parameter(s) based on step size."
  [name fx-node pname delta]
  (let [all     (audio/param-map-synth
                  (audio/fx-get name fx-node)
                  pname)
        min-val (:min all)
        max-val (:max all)
        step    (:step all)
        nodes   (if (sequential? fx-node) fx-node [fx-node])]
      (dorun (map
        #(util/delta
          [name % pname]
          param
          param!
          min-val
          max-val
          step
          delta)
        nodes))))

(defn get-index
  "Get the index of an fx node."
  [name fx-node]
  (.indexOf (audio/fx-list name) fx-node))

(defn move-before!
  "Switch fx node with its preceding node."
  [name fx-node]
  (let [index  (get-index name fx-node)
        before (dec index)]
    (when (and (>= index 1) (< index (count (audio/fx-list name))))
      (swap! audio/fx-store* update-in [name] util/swap index before)
      (let [other-node  (:node (get (get @audio/fx-store* name) index))
            other-nodes (if (sequential? other-node) other-node [other-node])
            nodes       (if (sequential? fx-node) fx-node [fx-node])]
        (dorun (map
          #(node-place* % :before (first other-nodes))
          nodes))))))

(defn move-after!
  "Switch fx node with its succeeding node."
  [name fx-node]
  (let [index (get-index name fx-node)
        after (inc index)]
    (when (and (>= index 0) (< index (dec (count (audio/fx-list name)))))
      (swap! audio/fx-store* update-in [name] util/swap index after)
      (let [other-node  (:node (get (get @audio/fx-store* name) index))
            other-nodes (if (sequential? other-node) other-node [other-node])
            nodes       (if (sequential? fx-node) fx-node [fx-node])]
        (dorun (map
          #(node-place* % :after (last other-nodes))
          (reverse nodes)))))))

(defn control-amt-get
  "Get control magnitude."
  [ctl-node]
  (audio/control-amt-get ctl-node))

(defn control-amt-set
  "Set control magnitude."
  [ctl-node amt]
  (audio/control-amt-set ctl-node amt))

(defn control-amt-reset
  "Set control magnitude to zero."
  [ctl-node]
  (audio/control-amt-reset ctl-node))

(defn control-amt-delta
  "Change fx control magnitude based on step size."
  [name fx-node pname ctl-node delta]
  (let [all     (audio/param-map-ctl
                  (audio/fx-get name fx-node)
                  pname)
        min-val (:min all)
        max-val (:max all)
        big-val (- max-val min-val)
        step    (:step all)
        nodes   (if (sequential? ctl-node) ctl-node [ctl-node])]
    (dorun (map
      #(util/delta
        [%]
        control-amt-get
        control-amt-set
        (* big-val -1)
        big-val
        step
        delta)
      nodes))))

(defn control-add!
  "Add a control bus to fx parameter(s)."
  [name fx-node pname bus]
  (let [nodes (if (sequential? fx-node) fx-node [fx-node])]
    (util/single (doall (map
      (fn [fx-n]
        (control/mix-add!
          audio/fx-control*
          [name fx-n pname]
          bus
          (control-nodes name fx-n pname)
          [fx-n]
          param))
      nodes)))))

(defn control-remove!
  "Remove a control node from an individual fx parameter."
  [name fx-n pname ctl-node]
  (let [nodes (if (sequential? ctl-node) ctl-node [ctl-node])]
    (util/single (dorun (map
      (fn [ctl-n]
        (control/mix-remove!
          audio/fx-control*
          [name fx-n pname]
          ctl-n
          (control-nodes name fx-n pname)
          [fx-n]
          param
          param!))
      nodes)))))

(defn control-clear!
  "Clear all control nodes from an fx parameter."
  [name fx-node pname]
  (dorun (map
    (fn [fx-n]
      (dorun (map
        #(control-remove! name fx-n pname %)
        (control-nodes name fx-n pname))))
    (if (sequential? fx-node) fx-node [fx-node]))))

(defn add!
  "Add fx to instrument."
  [name fx]
  (let [inst    (audio/inst-get name)
        fx-node (inst-fx! inst fx)]
    (swap! audio/fx-store* update-in [name] conj {:node fx-node :synth fx})
    (when
      (audio/inst-io-swap!
        name
        (-> (audio/inst-io name)
          (audio/inst-io-into :in (in-busses name fx-node))
          (audio/inst-io-into :out (out-busses name fx-node))))
      (audio/sort-node-tree!))
    fx-node))

(defn remove!
  "Remove fx from instrument."
  [name fx-node]
  (dorun (map
    (partial control-clear! name fx-node)
    (param-list name fx-node)))
  (swap! audio/fx-store* update-in [name] (partial remove #(= (:node %) fx-node)))
  (dorun (map #(node-free* %) (if (sequential? fx-node) fx-node [fx-node])))
  (audio/inst-io-swap!
    name
    (-> (audio/inst-io name)
      (audio/inst-io-remove :in (in-busses name fx-node))
      (audio/inst-io-remove :out (out-busses name fx-node))))
  nil)
