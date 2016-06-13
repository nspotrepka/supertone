(defproject supertone "0.1.0-SNAPSHOT"
  :description "A high-level music workstation for live production."
  :url "https://github.com/nspotrepka/supertone"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :scm {:name "git"
        :url "https://github.com/nspotrepka/supertone.git"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [overtone "0.9.1"]
  				       [seesaw "1.4.5"]
                 [com.evocomputing/colors "1.0.3"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]]
                   :source-paths ["dev" "live"]}})
