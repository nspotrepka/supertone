(ns supertone.studio.sampler
  (:require [overtone.sc.buffer :refer :all]
            [overtone.sc.sample :refer :all]
            [supertone.util     :as util]))

(defrecord Sampler [sample-vec])

(def sample-vec* (atom nil))

(defn init
  [s]
  (map->Sampler {
    :sample-vec (util/swap-or sample-vec* (:sample-vec s) [])}))

(defn dispose
  [s]
  (map->Sampler {
    :sample-vec (or @sample-vec* (:sample-vec s))}))

(def nil-id -3000)

;; add
;; load
;; generate
;; record

;; sample vs. buffer

;; duplicate, split, crop, remove
