(defproject lark/editors "0.1.0-SNAPSHOT"

  :url "https://www.github.com/braintripping/lark/tree/master/editors"

  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[re-view "0.3.29"]]

  :source-paths ["src"]

  :lein-release {:deploy-via :clojars
                 :scm        :git}

  :cljsbuild {:builds []})
