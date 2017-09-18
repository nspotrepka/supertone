(ns supertone.studio.sampler
  (:require [overtone.sc.buffer         :refer :all]
            [overtone.sc.sample         :refer :all]
            [overtone.samples.freesound :refer :all]
            [supertone.util             :as util]))

(defrecord Sampler [buf-store coll-store])

(def buf-store* (atom nil))
(def coll-store* (atom nil))

(defn init
  [s]
  (map->Sampler {
    :buf-store  (util/swap-or buf-store* (:buf-store s) {})
    :coll-store (util/swap-or coll-store* (:coll-store s) {})}))

(defn dispose
  [s]
  (map->Sampler {
    :buf-store  (or @buf-store* (:buf-store s))
    :coll-store (or @coll-store* (:coll-store s))}))

(def nil-id -3000)

(defn buf-list
  "List buffer ids."
  []
  (or (keys @buf-store*) '()))

(defn buf-get
  "Get buffer by id."
  [id]
  (get @buf-store* id))

(defn add!
  "Add a buffer. Returns buffer id."
  [buf]
  (let [id (:id buf)]
    (swap! buf-store* assoc id buf)
    id))

(defn remove!
  "Remove a buffer by id. Frees the buffer."
  [id]
  (let [buf (get buf-store* id)]
    (buffer-free buf)
    (swap! buf-store* dissoc id)
    nil))

(defn new!
  "Allocate a new buffer."
  ([frames] (new! frames 1))
  ([frames n-channels] (add! (buffer frames n-channels))))

(defn file!
  "Load an audio file."
  [path & args]
  (add! (apply load-sample path args)))

(defn freesound!
  "Load a sound by freesound id. Requires Freesound APIv2 authentication."
  [id & args]
  (add! (apply freesound id args)))

;; generate
;; record

(defn buf-read
  "Read the contents of a buffer. This function is slow for large buffers."
  [id]
  (buffer-read (buffer-get id)))

(defn coll-list
  "List buffer collection names."
  []
  (or (keys @coll-store*) '()))

(defn coll-get
  "Get a buffer collection by name."
  [name]
  (get @coll-store* name))

(defn coll-add!
  "Add a buffer collection."
  [name]
  (when (coll-get name)
    (throw (Exception. (str "Buffer collection already exists: " name))))
  (swap! coll-store* assoc name [])
  [])

(defn coll-rename!
  "Rename a buffer collection."
  [name new-name]
  (when (coll-get new-name)
    (throw (Exception. (str "Buffer collection already exists: " new-name))))
  (let [coll (coll-get name)]
    (swap! coll-store* dissoc name)
    (swap! coll-store* assoc new-name coll)
    coll))

(defn coll-duplicate!
  "Duplicate a buffer collection."
  [name new-name]
  (when (coll-get new-name)
    (throw (Exception. (str "Buffer collection already exists: " new-name))))
  (let [coll (coll-get name)]
    (swap! coll-store* assoc new-name coll)
    coll))

(defn coll-remove!
  "Remove a buffer collection."
  [name]
  (swap! coll-store* dissoc name)
  nil)

(defn coll-insert!
  "Insert a buffer id into a buffer collection."
  [name index id]
  (let [[head tail] (split-at index (coll-get name))
        new-coll    (into [] (concat head (cons id tail)))]
    (swap! coll-store* assoc name new-coll)
    new-coll))

(defn coll-delete!
  "Delete a buffer id from a buffer collection."
  [name index]
  (let [[head tail] (split-at index (coll-get name))
        new-coll    (into [] (concat head (rest tail)))]
    (swap! coll-store* assoc name new-coll)
    new-coll))

(defn coll-move!
  "Move a buffer id within a buffer collection."
  [name index new-index]
  (let [id (get (coll-get name) index)]
    (coll-delete! name index)
    (coll-insert! name new-index id)))

(defn coll-clear!
  "Clear all buffer ids from a buffer collection."
  [name]
  (when-not (coll-get name)
    (throw (Exception. (str "Buffer does not exist: " name))))
  (swap! coll-store* assoc name [])
  [])

;; swap
;; map
;; filter
