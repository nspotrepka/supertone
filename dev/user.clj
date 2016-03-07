(ns user)

(defn dev
  []
  (require 'dev :reload)
  (in-ns 'dev)
  :dev)

(defn live000
  []
  (require 'live000 :reload)
  (in-ns 'live000)
  :live000)
