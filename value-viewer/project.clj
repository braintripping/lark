(defproject lark/value-viewer "0.1.1-SNAPSHOT"

  :url "https://www.github.com/braintripping/lark/tree/master/value-viewer"

  :description "Navigable views for Clojure data structures."

  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.854"]
                 [re-view "0.3.29"]]

  :source-paths ["src"]

  :lein-release {:deploy-via :clojars
                 :scm        :git}

  :cljsbuild {:builds []})
