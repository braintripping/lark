(defproject lark/tree "0.1.3-SNAPSHOT"
  :description "Parsing tool for editors"
  :url "https://github.com/braintripping/lark/tree/master/tree"
  :license {:name "GNU Affero General Public License v3.0"
            :url  "https://www.gnu.org/licenses/agpl-3.0.txt"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.671"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [fast-zip "0.7.0"]
                 [net.cgrand/macrovich "0.2.0"]
                 [lark/util "0.1.0-SNAPSHOT"]
                 ]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-doo "0.1.6"]]

  :lein-release {:deploy-via :clojars}

  :doo {:build "test"}

  :cljsbuild {:builds [{:id           "test"
                        :source-paths ["src" "test"]
                        :compiler     {:output-to     "target/test.js"
                                       :output-dir    "target/test"
                                       :main          lark.tree.test-runner
                                       :optimizations :none}}]}

  :source-paths ["src"])
