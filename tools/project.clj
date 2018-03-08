(defproject lark/tools "0.1.28-SNAPSHOT"

  :description "Open-source components of the Lark editor"

  :url "https://github.com/braintripping/lark/tree/master/tree"

  :license {:name "GNU Affero General Public License v3.0"
            :url "https://www.gnu.org/licenses/agpl-3.0.txt"}

  :min-lein-version "2.7.1"

  :dependencies ~(into '[[org.clojure/clojure "1.9.0-alpha14"]
                         [org.clojure/clojurescript "1.9.671"]
                         [org.clojure/core.match "0.3.0-alpha4"]]
                   (->>
                     (slurp "deps.edn")
                     (read-string)
                     (:deps)
                     (reduce-kv (fn [deps k v]
                                  (conj deps [k (:mvn/version v)])) [])))

  :lein-release {:deploy-via :clojars
                 :scm :git}

  :source-paths ["src" "test"])
