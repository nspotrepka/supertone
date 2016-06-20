(ns user)

(defn dev
  "Go to dev namespace."
  []
  (require 'dev :reload)
  (in-ns 'dev)
  :dev)

(defn sets
  "List all sets in live folder."
  []
  (let [directory (clojure.java.io/file "live")
        fseq      (file-seq directory)]
    (map #(clojure.string/replace (subs % 5 (- (count %) 4)) "/" ".")
      (filter
        (fn [s] (and (.startsWith s "live/") (.endsWith s ".clj")))
        (map str fseq)))))

(defn live
  "Go to a live set namespace."
  [set]
  (let [sym (symbol (str "live." set))]
    (require sym :reload)
    (in-ns sym)
    sym))
