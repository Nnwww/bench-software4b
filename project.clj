(defproject bench-software4b "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [jansi-clj "0.1.1"]
                 ]
  :java-source-paths ["/Users/nnwww/WorkSpace/software_correct/tut2017informationQuantity/"]
  :main ^:skip-aot bench-software4b.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
