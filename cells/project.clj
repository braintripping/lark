(defproject lark/cells "0.1.4-SNAPSHOT"
  :description "Interactive async in ClojureScript."
  :url "https://www.github.com/braintripping/lark/tree/master/cells"
  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.854"]
                 [com.stuartsierra/dependency "0.2.0"]]

  :source-paths ["src"]
  :plugins [[lein-figwheel "0.5.13"]
            [lein-cljsbuild "1.1.7"]]
  :cljsbuild {:builds [{:id           "tests"
                        :source-paths ["src"
                                       "test"]
                        :figwheel     {:on-jsload "cells.tests-runner/run-tests"}
                        :compiler     {:main          cells.tests-runner
                                       :output-to     "test-target/public/js/tests.js"
                                       :output-dir    "test-target/public/js/out"
                                       :asset-path    "js/out"
                                       :optimizations :none}}]}
  :lein-release {:deploy-via :clojars
                 :scm        :git})
