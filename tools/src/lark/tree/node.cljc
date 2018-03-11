(ns lark.tree.node
  (:require [lark.tree.reader :as rd]
            [fast-zip.core :as z]
    #?(:clj
            [lark.tree.util :refer [contains-identical-keyword?]]))
  #?(:cljs (:require-macros [lark.tree.util :refer [contains-identical-keyword?]])))

(defn comment?
  "Returns true if node is a comment - either `;` or `#_` but not `(comment ...)`"
  [node]
  (contains-identical-keyword? [:comment :uneval]
                               (get node :tag)))

(defn whitespace?
  [node]
  (rd/whitespace-tag? (get node :tag)))

(defn newline?
  [node]
  (= :newline (get node :tag)))

(def sexp?
  "Returns false if node does not have corresponding s-expression (eg. comments and whitespace)"
  (every-pred (complement comment?)
              (complement whitespace?)))

(defn terminal-node? [node]
  (contains-identical-keyword? [:string
                                :number
                                :symbol
                                :token
                                :keyword
                                :space
                                :newline
                                :comma
                                :comment
                                :comment-block
                                :unmatched-delimiter]
                               (get node :tag)))

(def may-contain-children? (complement terminal-node?))

(defn has-edges?
  "Returns true if node has 'edges' that mark boundaries. See unwrap/edges for details."
  [node]
  (contains? rd/edges (get node :tag)))

(defn edges [node]
  (get rd/edges (get node :tag)))

(defn ast-zip
  "Given AST, returns zipper"
  [ast]
  (z/zipper
   may-contain-children?
   (fn [{:keys [children]}] (when-not (empty? children) children))
   (fn [node children] (assoc node :children children))
   ast))