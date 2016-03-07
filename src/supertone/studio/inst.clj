(ns supertone.studio.inst
  (:use overtone.core))

(defn init-instruments
  "Load the default instruments."
  []
  (println "--> Loading default instruments...")
  (use 'overtone.inst.synth))

(defn visible?
  "Checks if instrument is visible."
  [inst]
  (not (nil? (get (:instruments @studio*) (:name inst)))))

(defn inst-show!
  "Add an instrument to the studio."
  [inst & args]
  (reset! studio* (update-in @studio* [:instruments] assoc (:name inst) inst)))

(defn inst-hide!
  "Remove an instrument from the studio."
  [inst & args]
  (reset! studio* (update-in @studio* [:instruments] dissoc (:name inst))))

(defn undefinst!
  "Undefine an instrument."
  [inst]
  (inst-hide! inst)
  (node-free* (to-sc-id (:group inst)))
  (ns-unmap *ns* (symbol (:name inst))))

(defn inst-get-bus
  "Get the output bus of an instrument."
  [inst]
  (node-get-controls (:mixer inst) [:out-bus]))

(defn inst-set-bus
  "Set the output bus of an instrument."
  [inst bus]
  (ctl (:mixer inst) :out-bus bus))

(defn get-name
  "Returns the name of the node with the id."
  [node]
  (if (inst? node)
    (str (:name node) " " (:id (:instance-group node)))
    (str (:name node) " " (:id node))))
