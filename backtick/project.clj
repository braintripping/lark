(defproject lark/backtick "0.1.0"
  :description "Assorted utilities"
  :url "https://github.com/braintripping/lark/tree/master/tree"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]

  :lein-release {:deploy-via :clojars
                 :scm        :git}

  :source-paths ["src"])
