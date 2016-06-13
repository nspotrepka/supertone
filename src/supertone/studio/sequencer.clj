(ns supertone.studio.sequencer
  (:require [overtone.music.pitch  :refer :all])
  (:require [supertone.util        :as util]))

(defrecord Sequencer [seq-map])

(defrecord Sequence [type sequence pattern param-fns pulse-length swing])

(def seq-map* (atom nil))

(defn init
  [s]
  (map->Sequencer {
    :seq-map (util/swap-or seq-map* (:seq-map s) {})}))

(defn dispose
  [s]
  (map->Sequencer {
    :seq-map (or @seq-map* (:seq-map s))}))

(defn- distribute-prepend
  [v n]
  (if (< (count v) (* 2 n))
    v
    (let [a (take n v)
          bc (drop n v)
          mid (- (count v) (* 2 n))
          b (take mid bc)
          c (drop mid bc)]
      (concat (map concat c a) b))))

(defn- distribute-append
  [v n]
  (if (< (count v) (* 2 n))
    v
    (let [a (take n v)
          bc (drop n v)
          mid (- (count v) (* 2 n))
          b (take mid bc)
          c (drop mid bc)]
      (concat (map concat a c) b))))

(defn- rotate
  [v]
  (loop [x (first v)
         r (rest v)]
    (if (zero? (first r))
      (recur (first r) (concat (rest r) [x]))
      (concat r [x]))))

(defn- offset
  [v]
  (concat (rest v) [(first v)]))

(defn bjorklund
  [steps pulses]
  (loop [seq (concat (repeat pulses [1]) (repeat (- steps pulses) [0]))
         n steps
         k pulses]
    (if (<= k 1)
      (flatten seq)
      (let [q (quot n k)
            m (mod n k)
            s0 (if (and (= m 0) (> (count (first seq)) 1))
                 (reduce distribute-prepend seq (repeat (- q 1) k))
                 (reduce distribute-append seq (repeat (- q 1) k)))
            s1 (if (<= m 1) s0 (distribute-append s0 m))]
        (recur s1 k m)))))

(defn bjorklund-plus
  [steps pulses rot off]
  (nth (iterate offset
    (nth (iterate rotate
      (bjorklund steps pulses)) rot)) off))

(defn bjorklund-multi
  [steps pulses-vec rotate-vec offset-vec]
  (map #(bjorklund-plus steps %1 %2 %3) pulses-vec rotate-vec offset-vec))

(defn seq-new
  "Create a new sequence."
  [type pattern pulse-length swing]
  (map->Sequence {
    :type         type
    :sequence     {}
    :pattern      pattern
    :param-fns    {}
    :pulse-length pulse-length
    :swing        swing}))

(defn seq-generate
  "Generate/regenerate a sequence from parameters."
  [seq]
  (let [pattern   (:pattern seq)
        param-fns (:param-fns seq)]
    (assoc seq
      :sequence
      (loop [n    0
             i    0
             x    (first pattern)
             r    (rest pattern)
             seq2 {}]
        (if x
          (if (zero? x)
            (recur n (+ i 1) (first r) (rest r) seq2)
            (recur (+ n 1) (+ i 1) (first r) (rest r)
              (reduce
                #(update seq2 i
                  assoc (first %2) ((second %2) n i)) seq2 param-fns)))
          seq2)))))

(defn seq-param
  "Add a param and its generative function to a sequence."
  [seq pname f]
  (assoc-in seq [:param-fns pname] f))

(defn seq-add!
  "Add a new sequence to the collection."
  [type pattern pulse-length swing]
  (let [seq (seq-new type pattern pulse-length swing)]
    (swap! seq-map* assoc seq [])
    seq))

(defn seq-replace!
  "Replace a sequence in the collection."
  [seq seq2]
  (let [v (or (get @seq-map* seq) [])]
    (swap! seq-map* dissoc seq)
    (swap! seq-map* assoc seq2 v)
    seq2))

(defn seq-add-inst!
  "Add an instrument to the sequence."
  [seq inst]
  (let [insts (get @seq-map* seq)]
    (swap! seq-map* assoc seq (conj insts inst))))

;; Compound bjorklund
;; Complex pattern generation (with repetition + syncopation)
;; Create sequence from pattern
;; Add param pattern to sequence
;; Remove param from sequence
