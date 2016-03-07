(ns supertone.studio.sequencer
  (:require [overtone.music.pitch  :refer :all])
  (:require [supertone.util        :as util]))

(defrecord Sequencer [seq-vec])

(def seq-vec* (atom nil))

(defn init
  [s]
  (map->Sequencer {
    :seq-vec (util/swap-or seq-vec* (:seq-vec s) [])}))

(defn dispose
  [s]
  (map->Sequencer {
    :seq-vec (or seq-vec* (:seq-vec s))}))

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

(defn rotate-reverse
  [v]
  (reverse (concat (rest v) [(first v)])))

(defn bjorklund
  [steps pulses]
  (loop [seq (concat (repeat pulses [1]) (repeat (- steps pulses) [0]))
         n steps
         k pulses]
    (if (<= k 1)
      (flatten seq)
      (let [q (quot n k)
            m (mod n k)
            s0 (if (= m 0)
                 (reduce distribute-prepend seq (repeat (- q 1) k))
                 (reduce distribute-append seq (repeat (- q 1) k)))
            s1 (if (<= m 1) s0 (distribute-append s0 m))]
        (recur s1 k m)))))
