(defproject lark/tools "0.1.9-SNAPSHOT"

  :description "Open-source components of the Lark editor"

  :url "https://github.com/braintripping/lark/tree/master/tree"

  :license {:name "GNU Affero General Public License v3.0"
            :url  "https://www.gnu.org/licenses/agpl-3.0.txt"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.671"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [re-view "0.4.0"]
                 [fast-zip "0.7.0"]
                 [net.cgrand/macrovich "0.2.0"]
                 [lark/backtick "0.1.0"]]

  :lein-release {:deploy-via :clojars
                 :scm        :git}

  :source-paths ["src"])
