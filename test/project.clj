(defproject lark/tests "0.1.0-SNAPSHOT"
  :description "Tests for the Lark editor"
  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.854"]
                 [lark/cells "0.1.0-SNAPSHOT"]
                 [lark/structure "0.1.0-SNAPSHOT"]
                 [lark/editors "0.1.0-SNAPSHOT"]]
  :source-paths ["src"]
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
