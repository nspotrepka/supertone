(ns supertone.studio.groups
  (:require [overtone.libs.deps                 :only [on-deps satisfy-deps]
                                                :refer :all]
            [overtone.sc.node                   :only [group]
                                                :refer :all]
            [overtone.sc.server                 :only [ensure-connected!]
                                                :refer :all]
            [overtone.sc.machinery.server.comms :only [with-server-sync]
                                                :refer :all]
            [overtone.sc.foundation-groups      :refer :all]
            [overtone.studio.core               :refer :all]
            [supertone.util                     :as util]))

(defrecord Groups [groups])

(def groups* (atom nil))

(defn- init-groups
  "Initialize Supertone groups in the node tree."
  []
  (let [control-group
        (with-server-sync
          #(group "Control" :head (foundation-root-group))
          "whilst creating the Supertone Control group")

        metro-group
        (with-server-sync
          #(group "Metronome" :head (foundation-root-group))
          "whilst creating the Supertone Metronome group")

        audio-dummy-group
        (with-server-sync
          #(group "Audio Dummy" :head (:instrument-group @studio*))
          "whilst creating the Supertone Audio Dummy group")

        control-dummy-group
        (with-server-sync
          #(group "Control Dummy" :head control-group)
          "whilst creating the Supertone Control Dummy group")]
    (assoc {}
      :control       control-group
      :metro         metro-group
      :audio-dummy   audio-dummy-group
      :control-dummy control-dummy-group)))

(defn init
  [s]
  (map->Groups {
    :groups (util/swap-or groups* (:groups s) (init-groups))}))

(defn dispose
  [s]
  (map->Groups {
    :groups (or @groups* (:groups s))}))

(defn control
  "Returns the control group.

   This group is for control synths."
  []
  (ensure-connected!)
  (:control @groups*))

(defn metro
  "Returns the metronome group.

   This group is for the metronome."
  []
  (ensure-connected!)
  (:metro @groups*))

(defn audio-dummy
  "Returns the audio dummy group.

   This group is for sorting the audio node tree."
  []
  (ensure-connected!)
  (:audio-dummy @groups*))

(defn control-dummy
  "Returns the control dummy group.

   This group is for sorting the control node tree."
  []
  (ensure-connected!)
  (:control-dummy @groups*))
