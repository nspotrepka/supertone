(ns supertone.studio.buffer
  (:require [overtone.sc.buffer :refer :all]
            [overtone.sc.sample :refer :all
            [supertone.util     :as util]))

(defrecord Buffer [buffer-vec])

(def buffer-vec* (atom nil))

(defn init
  [s]
  (map->Buffer {
    :buffer-vec (util/swap-or buffer-vec* (:buffer-vec s) [])}))

(defn dispose
  [s]
  (map->Audio {
    :buffer-vec (or @buffer-vec* (:buffer-vec s))}))

(def nil-id -3000)

;; add
;; load
;; generate
;; record

;; sample vs. buffer

;; duplicate, split, crop, remove
