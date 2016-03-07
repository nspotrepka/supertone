(ns supertone.util)

(defn list-inst
  "List all instruments."
  []
  (->> 'supertone.core
    (ns-map)
    (vals)
    (filter #(= (type %) :overtone.studio.inst/instrument))
    (sort-by str)))

(defn list-synth
  "List all synths."
  []
  (->> 'supertone.core
    (ns-map)
    (vals)
    (filter #(= (type %) :overtone.sc.synth/synth))
    (sort-by str)))

(defn list-cgen
  "List all composite generators."
  []
  (->> 'supertone.core
    (ns-map)
    (vals)
    (filter #(= (type %) :overtone.sc.defcgen/cgen))
    (sort-by str)))

(defmacro swap-or
  "Swaps an atom with the first element that is not nil."
  ([x] @x)
  ([x & next]
   `(let [a# ~x]
      (swap! a# #(if % % (or ~@next))))))

(defn swap [v i j]
  (assoc v j (v i) i (v j)))

(defn first-of [coll]
  (if (vector? coll) (first coll) coll))

(defn map-invert
  "Get all the input/output instruments to/from a bus."
  [io-map]
  (let [names   (keys io-map)
        fn-in   (fn
                  [map name busses]
                  (reduce
                    #(update-in %1 [%2 :in]
                      (fn [v e] (if (nil? v) [e] (conj v e)))
                        name) map busses))
        fn-out  (fn
                  [map name busses]
                  (reduce
                    #(update-in %1 [%2 :out]
                      (fn [v e] (if (nil? v) [e] (conj v e)))
                        name) map busses))
        io-in   (reduce #(fn-in %1 %2 (get-in io-map [%2 :out])) {} names)
        io-all  (reduce #(fn-out %1 %2 (get-in io-map [%2 :in])) io-in names)]
    io-all))
