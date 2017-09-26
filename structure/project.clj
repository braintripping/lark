(defproject lark/structure "0.1.0-SNAPSHOT"

  :url "https://www.github.com/braintripping/lark/tree/master/structure"

  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :dependencies [[fast-zip "0.7.0"]
                 [magic-tree "0.0.14-SNAPSHOT"]
                 [cljsjs/codemirror "5.19.0-0"]
                 [lark/commands "0.1.1-SNAPSHOT"]]

  :source-paths ["src"]

  :lein-release {:deploy-via :clojars
                 :scm        :git}

  :cljsbuild {:builds []})