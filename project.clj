(defproject cloisterjs "0.1.0-SNAPSHOT"
  :description "ClojureScript videogame library"
  :url ""
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [ring "1.1.8"]]
  :plugins [[lein-cljsbuild "0.3.2"]
            [lein-ring "0.8.3"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild { 
    :builds {
      :main {
        :source-paths ["src/cljs"]
        :compiler {:output-to "output/cljs.js"
                   :optimizations :simple
                   :pretty-print true}
        :jar true}}}
)
