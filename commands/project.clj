(defproject lark/commands "0.1.1-SNAPSHOT"

  :url "https://www.github.com/braintripping/lark/tree/master/commands"

  :description "Library for mapping keybindings to context-sensitive commands."

  :license {:name "Mozilla Public License 2.0"
            :url  "https://www.mozilla.org/en-US/MPL/2.0/"}

  :source-paths ["src"]

  :lein-release {:deploy-via :clojars
                 :scm        :git}

  :cljsbuild {:builds []})
