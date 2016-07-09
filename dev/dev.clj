(ns dev
  (:require
   [clojure.java.javadoc         :refer [javadoc]]
   [clojure.pprint               :refer [pprint]]
   [clojure.reflect              :refer [reflect]]
   [clojure.repl                 :refer [apropos dir doc find-doc pst source]]
   [clojure.tools.namespace.repl :refer [disable-unload! refresh refresh-all]]
   [overtone.live                :refer :all]
   [overtone.sc.machinery.server.comms :only [with-server-sync]
                                       :refer :all]
   [supertone.core               :as core]
   [supertone.util               :as util]
   [supertone.studio.bus         :as bus]
   [supertone.studio.groups      :as groups]
   [supertone.studio.metro       :as metro]
   [supertone.studio.control     :as control]
   [supertone.studio.audio       :as audio]
   [supertone.studio.sampler     :as sampler]
   [supertone.studio.sequencer   :as sequencer]
   [supertone.studio.midi        :as midi]
   [supertone.view.gui           :as gui]
   [supertone.midi.beatstep      :as beatstep]
   [supertone.midi.launchpad-mk2 :as launchpad]))

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

(defsynth sineboy
  [freq 4
   out-bus -1000]
  (out:kr out-bus (sin-osc:kr freq)))

(defn f1
  []
  (let [i  (audio/inst-add! "sawboy" "seesaw")
        b  (control/ctl-add! sineboy :freq 1)
        b2 (control/ctl-add! sineboy :freq 10)
        n  (audio/inst-node-add! "seesaw")
        x  (audio/inst-param-delta! "seesaw" "freq" 1)
        y  (audio/inst-param-reset! "seesaw" "freq")
        z  (audio/inst-param! "seesaw" "freq" 200)
        c  (audio/inst-ctl-add! "seesaw" "freq" b)
        c2 (audio/inst-ctl-add! "seesaw" "freq" b2)]
    (audio/inst-ctl-amt-delta "seesaw" "freq" c 1)
    (audio/inst-ctl-amt-delta "seesaw" "freq" c2 1)
    (audio/ctl-amt-reset c)
    (audio/ctl-amt-reset c2)
    (audio/ctl-amt-set c 50)
    (audio/ctl-amt-set c2 10)))

(defn f2
  []
  (let [i  (audio/inst-get "seesaw")
        nn (audio/inst-ctl-nodes "seesaw" "freq")
        b  (first nn)
        b2 (second nn)]
    (audio/inst-ctl-remove! "seesaw" "freq" b2)
    (audio/inst-ctl-remove! "seesaw" "freq" b)))

(defn f3
  []
  (let [i  (audio/inst-get "seesaw")
        nn (audio/inst-ctl-nodes "seesaw" "freq")
        b  (first nn)
        b2 (second nn)]
    (audio/inst-remove! "seesaw")
    (dorun (map #(control/ctl-remove! %) (keys @control/ctl-map*)))))

(defn f4
  []
  (let [i  (audio/inst-add! "sawboy" "dragon")
        f  (audio/fx-add! "dragon" fx-rlpf)
        b  (control/ctl-add! sineboy :freq 1)
        b2 (control/ctl-add! sineboy :freq 30)
        n  (audio/inst-node-add! "dragon")
        x  (audio/fx-param-delta! "dragon" f "cutoff" 200)
        y  (audio/fx-param-reset! "dragon" f "cutoff")
        z  (audio/fx-param! "dragon" f "cutoff" 1200)
        c  (audio/fx-ctl-add! "dragon" f "cutoff" b)
        c2 (audio/fx-ctl-add! "dragon" f "cutoff" b2)]
    (audio/fx-ctl-amt-delta "dragon" f "cutoff" c 400)
    (audio/fx-ctl-amt-delta "dragon" f "cutoff" c2 200)
    (audio/ctl-amt-reset c)
    (audio/ctl-amt-reset c2)
    (audio/ctl-amt-set c 400)
    (audio/ctl-amt-set c2 200)))

(defn f5
  []
  (let [i (audio/inst-get "dragon")
        f (audio/fx-nodes "dragon")]
    (audio/fx-param! "dragon" f "cutoff" 600)
    (dorun
      (map
        #(audio/fx-ctl-remove! "dragon" % "cutoff"
          (audio/fx-ctl-nodes "dragon" % "cutoff"))
        f))
    (audio/fx-param "dragon" f "cutoff")))

(defn f6
  []
  (let [i (audio/inst-get "dragon")
        f (first (audio/fx-nodes "dragon"))]
    (audio/inst-remove! "dragon")
    (dorun (map #(control/ctl-remove! %) (keys @control/ctl-map*)))))
