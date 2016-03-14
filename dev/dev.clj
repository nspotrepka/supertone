(ns dev
  (:require
   [clojure.java.javadoc         :refer [javadoc]]
   [clojure.pprint               :refer [pprint]]
   [clojure.reflect              :refer [reflect]]
   [clojure.repl                 :refer [apropos dir doc find-doc pst source]]
   [clojure.tools.namespace.repl :refer [disable-unload! refresh refresh-all]]
   [overtone.live                :refer :all]
   [supertone.core               :as core]
   [supertone.util               :as util]
   [supertone.studio.bus         :as bus]
   [supertone.studio.groups      :as groups]
   [supertone.studio.metro       :as metro]
   [supertone.studio.control     :as control]
   [supertone.studio.audio       :as audio]
   [supertone.studio.sequencer   :as sequencer]))

(disable-unload!)

(defonce session* (atom nil))

(defn user
  "Switches back to the user namespace."
  []
  (in-ns 'user)
  :user)

(defn- init
  "Initializes the session, syncing if already initialized."
  []
  (swap! session* core/init))

(defn- dispose
  "Cleans up the session."
  []
  (swap! session* core/dispose))

(defn- ns-unalias-all
  "Removes all aliases from all namespaces."
  []
  (doall (flatten (map
    (fn [n] (map
      (partial ns-unalias n)
      (keys (ns-aliases n))))
    (all-ns)))))

(defn reset
  "Disposes the system, reloads modified source files, syncs the system."
  []
  (dispose)
  (ns-unalias-all)
  (refresh :after `go))

(defn reset-all
  "Disposes the system, reloads all source files, syncs the system."
  []
  (dispose)
  (ns-unalias-all)
  (refresh-all :after `go))

(defn go
  "Just go."
  []
  (init)
  :ready)

(defn test-sortable
  []
  (let [n      100
        busses (map (fn [_] (bus/audio)) (range n))
        insts  (shuffle
                 (map
                   #(:name (audio/inst-add! "mono-pass" (str "NOTICE ME " %)))
                   (range n)))]
    (doall (map #(audio/inst-in! %1 (:id %2)) insts busses))
    (doall (map #(audio/inst-out! %1 (:id %2)) (butlast insts) (rest busses)))
    (audio/inst-out! (last insts) 0.0)
    insts))
