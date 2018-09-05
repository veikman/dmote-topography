(defproject dmote-topography "0.2.0"
  :description "DMOTE wrist rest topography generator"
  :url "http://viktor.eikman.se/article/the-dmote/"
  :license {:name "GPL"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.7"]
                 [net.mikera/core.matrix "0.62.0"]
                 [unicode-math "0.2.1"]
                 [environ "1.1.0"]]
  :plugins [[lein-environ "1.1.0"]]
  :main ^:skip-aot dmote-topography.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
