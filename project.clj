(defproject verify-publications "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.9.2"]
                 [org.clojure/data.json "0.2.5"]
                 [enlive "1.1.5"]
                 [org.clojure/data.csv "0.1.2"]
                 ]
  :main ^:skip-aot verify-publications.core
  :target-path "target/%s"
  ; :jvm-opts ["-Dhttp.proxyHost=127.0.0.1" "-Dhttp.proxyPort=8123"]
  :jvm-opts ["-Dhttp.proxyHost=127.0.0.1" "-Dhttp.proxyPort=8080"]
  :profiles {:uberjar {:aot :all}})
