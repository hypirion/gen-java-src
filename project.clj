(defproject gen-java-src "0.1.0-SNAPSHOT"
  :description "Generate Java code in the browser"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0-RC1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [com.cemerick/clojurescript.test "0.2.2"]
                 [com.cemerick/double-check "0.5.6-SNAPSHOT"]
                 [hiccups "0.3.0"]
                 [domina "1.0.2"]]
  :plugins [[lein-cljsbuild "1.0.2"]]
  :cljsbuild {:builds
              {:development
               {:source-paths ["src/cljs"]
                :compiler {:output-to "out/dev/gen-java.js"
                           :output-dir "out/dev"
                           :optimizations :whitespace
                           :pretty-print true
                           :libs [""]}}
               :production
               {:source-paths ["src/cljs"]
                :compiler {:output-to "out/production/gen-java.js"
                           :output-dir "production"
                           :optimizations :simple
                           :pretty-print false
                           :libs [""]}}}})
