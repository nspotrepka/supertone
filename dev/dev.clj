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
   [supertone.studio.inst        :as inst]
   [supertone.studio.fx          :as fx]
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
                   #(:name (inst/add! "mono-pass" (str "NOTICE ME " %)))
                   (range n)))]
    (doall (map #(inst/in-bus! %1 (:id %2)) insts busses))
    (doall (map #(inst/out-bus! %1 (:id %2)) (butlast insts) (rest busses)))
    (inst/out-bus! (last insts) 0.0)
    insts))

(defsynth sineboy
  [freq 4
   out-bus -1000]
  (out:kr out-bus (sin-osc:kr freq)))

(defn f1
  []
  (let [i  (inst/add! "sawboy" "seesaw")
        b  (control/add! sineboy :freq 1)
        b2 (control/add! sineboy :freq 10)
        n  (inst/node-add! "seesaw")
        x  (inst/param-delta! "seesaw" "freq" 1)
        y  (inst/param-reset! "seesaw" "freq")
        z  (inst/param! "seesaw" "freq" 200)
        c  (inst/control-add! "seesaw" "freq" b)
        c2 (inst/control-add! "seesaw" "freq" b2)]
    (inst/control-amt-delta "seesaw" "freq" c 1)
    (inst/control-amt-delta "seesaw" "freq" c2 1)
    (inst/control-amt-reset c)
    (inst/control-amt-reset c2)
    (inst/control-amt-set c 50)
    (inst/control-amt-set c2 10)))

(defn f2
  []
  (let [i  (audio/inst-get "seesaw")
        nn (inst/control-nodes "seesaw" "freq")
        b  (first nn)
        b2 (second nn)]
    (inst/control-remove! "seesaw" "freq" b2)
    (inst/control-remove! "seesaw" "freq" b)))

(defn f3
  []
  (let [i  (audio/inst-get "seesaw")
        nn (inst/control-nodes "seesaw" "freq")
        b  (first nn)
        b2 (second nn)]
    (inst/remove! "seesaw")
    (dorun (map #(control/remove! %) (control/control-list)))))

(defn f4
  []
  (let [i  (inst/add! "sawboy" "dragon")
        f  (fx/add! "dragon" fx-rlpf)
        b  (control/add! sineboy :freq 1)
        b2 (control/add! sineboy :freq 30)
        n  (inst/node-add! "dragon")
        x  (fx/param-delta! "dragon" f "cutoff" 200)
        y  (fx/param-reset! "dragon" f "cutoff")
        z  (fx/param! "dragon" f "cutoff" 1200)
        c  (fx/control-add! "dragon" f "cutoff" b)
        c2 (fx/control-add! "dragon" f "cutoff" b2)]
    (fx/control-amt-delta "dragon" f "cutoff" c 400)
    (fx/control-amt-delta "dragon" f "cutoff" c2 200)
    (fx/control-amt-reset c)
    (fx/control-amt-reset c2)
    (fx/control-amt-set c 400)
    (fx/control-amt-set c2 200)))

(defn f5
  []
  (let [i (audio/inst-get "dragon")
        f (first (audio/fx-list "dragon"))]
    (fx/param! "dragon" f "cutoff" 600)
    (dorun
      (map
        #(fx/control-remove! "dragon" % "cutoff"
          (fx/control-nodes "dragon" % "cutoff"))
        f))
    (fx/param "dragon" f "cutoff")))

(defn f6
  []
  (let [i (audio/inst-get "dragon")
        f (first (audio/fx-list "dragon"))
        g (fx/add! "dragon" fx-distortion)]
    (fx/move-after! "dragon" f)
    (println (> (fx/get-index "dragon" f) (fx/get-index "dragon" g)))
    (fx/move-before! "dragon" f)
    (println (< (fx/get-index "dragon" f) (fx/get-index "dragon" g)))
    (fx/remove! "dragon" g)))

(defn f7
  []
  (let [i (audio/inst-get "dragon")
        f (first (audio/fx-list "dragon"))]
    (inst/remove! "dragon")
    (dorun (map #(control/remove! %) (control/control-list)))))
