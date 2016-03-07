(ns supertone.gui.icons
  (:require [quil.core :as q]))

(defn icon-rarrow
  [x y]
  (q/begin-shape)
  (q/vertex (+ x 7) (+ y 5))
  (q/vertex (+ x 12) (+ y 10))
  (q/vertex (+ x 7) (+ y 15))
  (q/end-shape :close))

(defn icon-square
  [x y]
  (q/begin-shape)
  (q/vertex (+ x 5) (+ y 5))
  (q/vertex (+ x 14) (+ y 5))
  (q/vertex (+ x 14) (+ y 14))
  (q/vertex (+ x 5) (+ y 14))
  (q/end-shape :close))
