(defproject lark/editors "0.1.2-SNAPSHOT"

  :url "https://www.github.com/braintripping/lark/tree/master/editors"

  :description "Protocols for editors."

  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.946"  :scope "provided"]
                 [re-view "0.3.29"]]

  :source-paths ["src"]

  :lein-release {:deploy-via :clojars
                 :scm        :git}

  :cljsbuild {:builds []})
