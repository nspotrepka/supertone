(ns supertone.util)

(defn list-type
  "List all variables of the specified type"
  [t]
  (->> *ns*
    (ns-map)
    (vals)
    (filter #(= (type %) t))
    (sort-by str)))

(defn list-synth
  "List all synths."
  []
  (list-type :overtone.sc.synth/synth))

(defn list-cgen
  "List all composite generators."
  []
  (list-type :overtone.sc.defcgen/cgen))

(defmacro swap-or
  "Swap an atom with the first argument that is not nil."
  ([x] @x)
  ([x & next]
   `(let [a# ~x]
      (swap! a# #(if % % (or ~@next))))))

(defn swap
  "Switch two elements in a vector."
  [v i j]
  (assoc v j (v i) i (v j)))

(defn first-of
  "If v is vector, return first element of v, else return v."
  [v]
  (if (vector? v) (first v) v))

(defn single
  "If collection has only one element, return its element."
  [coll]
  (if (= (count coll) 1) (first coll) coll))

(defn map-invert
  "If instrument i/o, return bus i/o, and vice versa."
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

(defn delta
  "Change parameter based on step size."
  [id param-get-fn param-set-fn param-min param-max param-step delta]
  (let [val (+ (apply param-get-fn id) (* param-step delta))]
    (apply param-set-fn
      (into id
        [(max (min val param-max) param-min)]))))
