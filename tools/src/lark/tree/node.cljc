(ns lark.tree.node
  (:require [lark.tree.emit :as unwrap]
            [fast-zip.core :as z]
    #?(:clj
            [lark.tree.util :refer [contains-identical-keyword?]]))
  #?(:cljs (:require-macros [lark.tree.util :refer [contains-identical-keyword?]])))

(defn comment?
  "Returns true if node is a comment - either `;` or `#_` but not `(comment ...)`"
  [node]
  (contains-identical-keyword? [:uneval :comment]
                               (get node :tag)))

(defn whitespace?
  [node]
  (contains-identical-keyword? [:space :newline :comma]
                               (get node :tag)))

(defn newline?
  [node]
  (identical? :newline (get node :tag)))

(def sexp?
  "Returns false if node does not have corresponding s-expression (eg. comments and whitespace)"
  (every-pred (complement comment?)
              (complement whitespace?)))

(defn terminal-node? [node]
  (contains-identical-keyword? [:string :token :symbol :regex :var :keyword :namespaced-keyword :space :newline :comma :comment :comment-block]
                               (get node :tag)))

(def may-contain-children? (complement terminal-node?))

(defn has-edges?
  "Returns true if node has 'edges' that mark boundaries. See unwrap/edges for details."
  [node]
  (contains? unwrap/edges (get node :tag)))

