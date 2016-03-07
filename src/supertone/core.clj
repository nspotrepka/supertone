(ns supertone.core
  (:require [overtone.live                :refer :all]
            [supertone.util               :as util]
            [supertone.studio.bus         :as bus]
            [supertone.studio.groups      :as groups]
            [supertone.studio.metro       :as metro]
            [supertone.studio.control     :as control]
            [supertone.studio.audio       :as audio]
            [supertone.studio.sequencer   :as sequencer]
            [quil.core                    :as q])
  (:gen-class :main true))

(defrecord Session [bus groups metro audio])

(defn init
  "Initializes Supertone, syncing if already initialized."
  ([] (init nil))
  ([s]
    ;; (boot-external-server)
    (ensure-connected!)
    (map->Session {
      :bus       (bus/init (:bus s))
      :groups    (groups/init (:groups s))
      :metro     (metro/init (:metro s))
      :control   (control/init (:control s))
      :audio     (audio/init (:audio s))
      :sequencer (sequencer/init (:sequencer s))})))

(defn dispose
  "Cleans up Supertone."
  ([] (dispose nil))
  ([s]
    ;; (dispose (:applet @gui*))
    (ensure-connected!)
    (map->Session {
      :bus       (bus/dispose (:bus s))
      :groups    (groups/dispose (:groups s))
      :metro     (metro/dispose (:metro s))
      :control   (metro/dispose (:control s))
      :audio     (audio/dispose (:audio s))
      :sequencer (sequencer/dispose (:sequencer s))})))

(defn -main
  "Not that useful."
  [& args]
  (println "Use the REPL."))
