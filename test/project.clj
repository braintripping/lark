(defproject lark/tests "0.1.0-SNAPSHOT"
  :description "Tests for the Lark editor"
  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.854"]
                 [org.clojure/core.match "0.3.0-alpha4"]


                 [lark/tree "0.1.3-SNAPSHOT"]
                 [re-view "0.3.31"]
                 [lark/commands "0.1.2-SNAPSHOT"]
                 [lark/value-viewer "0.1.2-SNAPSHOT"]
                 [lark/cells "0.1.5-SNAPSHOT"]
                 [lark/structure "0.1.3-SNAPSHOT"]
                 [lark/editors "0.1.3-SNAPSHOT"]
                 [lark/tree "0.1.3-SNAPSHOT"]]
  :source-paths ["src"
                 "../tree/src"
                 "../structure/src"
                 "../editors/src"
                 "../commands/src"
                 "../cells/src"]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.7"]]
  :doo {:build "tests"}
  :cljsbuild {:builds [{:id           "tests"
                        :source-paths ["src"]
                        :figwheel     {:on-jsload "tests.tests-runner/run-tests"}
                        :compiler     {:main          tests.tests-runner
                                       :output-to     "test-target/public/js/tests.js"
                                       :output-dir    "test-target/public/js/out"
                                       :asset-path    "js/out"
                                       :optimizations :none}}]})
