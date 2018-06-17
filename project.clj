(defproject dmote-topology "0.1.0-SNAPSHOT"
  :description "DMOTE wrist rest topology generator"
  :url "http://viktor.eikman.se/article/the-dmote/"
  :license {:name "GPL"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.7"]
                 [unicode-math "0.2.1"]]
  :main ^:skip-aot dmote-topology.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
