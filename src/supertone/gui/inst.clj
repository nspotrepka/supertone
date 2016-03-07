(ns supertone.gui.inst
  (:require [quil.core :as q])
  (:use supertone.gui.util))

(comment

(defn node-sliders
  [group y params name]
  (let [p (first params)
        r (rest params)]
    (-> @gui-cp5*
      (.addSlider (str name "/" p))
      (.setBroadcast false)
      (.setPosition 0 y)
      (.setSize 200 20)
      (.setRange 100 300)
      (.moveTo group)
      (.getCaptionLabel)
      (.setText p))
    (when (seq r) (node-sliders group (+ 20 y) r name))))

(defn node-group
  [node name]
  (let [g (-> @gui-cp5* (.addGroup name))]
    (node-sliders g 0 (:args node) name)
    g))

(defn inst-tab
  []
  (add-tab "instruments")
  (reset! node-accordion*
    (-> @gui-cp5*
      (.addAccordion "node-accordion")
      (.setPosition 20 42)
      (.moveTo "instruments")))
  (doseq [inst (sort-by :name (vals (:instruments @studio*)))]
    (let [name (get-name inst)
          g (node-group inst name)]
      (-> g
        (.setWidth 40)
        (.setHeight 20)
        (.getCaptionLabel)
        (.setFont @font-cp5*))
      (.addItem @node-accordion* g)
      (assoc @node-gui* name g)
      (doseq [fx (:children (node-tree (:fx-group inst)))]
        (let [name2 (str (:name inst) "/" (get-name fx))
              g2 (node-group fx name2)]
          (-> g2
            (.setHeight 20)
            (.getCaptionLabel)
            (.setWidth 600)
            (.setFont @font-cp5*))
          (assoc @node-gui* name2 g2)
          (.addItem @node-accordion* g2))))))

(defn inst-tab-draw
  [g]
  (when (.isActive (.getTab @gui-cp5* "instruments"))
    (q/fill (get-color :t1))
    (q/no-stroke)
    (icon-rarrow 0 63)
    (icon-square 0 42)))

  )
