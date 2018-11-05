(ns lark.tree.core
  (:require [lark.tree.parse :as parse]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.tree.format :as format]
            [fast-zip.core :as z]))

;; Parse

(def ast
  "Given ClojureScript source, returns AST"
  parse/ast)

(def ast-zip n/ast-zip)

(def string-zip
  "Given ClojureScript source, returns zipper"
  (comp ast-zip parse/ast))

;; Navigation
(comment
 (defn format-old [x]
   (let [x (cond-> x
                   (string? x) (parse/ast))]
     (binding [format/*pretty* true]
       (emit/string-old x)))))

(defn format-zip [loc]
  {:pre [(= (type loc) z/ZipperLocation)]}
  (-> loc
      (format/format-zip)
      (emit/zip)))

(defn format-string [s]
  {:pre [(string? s)]}
  (-> (string-zip s)
      (format-zip)
      .-node
      :string))

#_(do

  (let [s ""]
    (let [_ (prn :parse-dirty-string)
          ast (do #_(dotimes [n 4]
                      (parse/ast s))
                (time (parse/ast s)))

          _ (prn :emit-dirty-string)
          _ (do #_(dotimes [n 4]
                      (emit/string ast))
                (time (emit/string ast)))

          _ (prn :format-dirty-string-new)
          _ (simple-benchmark [] (format-string s) 1)

          s (format-string s)

          _ (prn :format-clean-string-new)
          _ (simple-benchmark [] (format-string s) 1)]
      (println :cljs-core-string-verify (= str s)))))
