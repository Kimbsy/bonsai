(defproject bonsai "0.1.0"
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [quip "3.0.1"]]
  :main ^:skip-aot bonsai.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
