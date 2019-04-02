(ns lark.tree.core
  (:require [lark.tree.parse :as parse]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.tree.format :as format]
            [lark.fast-zip :as z]))

;; Parse

(def ast
  "Given ClojureScript source, returns AST"
  parse/ast)

(def zip n/ast-zip)

(def string-zip
  "Given ClojureScript source, returns zipper"
  (comp zip parse/ast))

(defn format-zip [loc]
  {:pre [(= (type loc) z/ZipperLocation)]}
  (-> (z/node loc)
      (emit/materialize {:format true})
      n/ast-zip))

(defn format-ast [node]
  (emit/materialize node {:format true}))

(defn format-string [s]
  {:pre [(string? s)]}
  (-> (parse/ast s)
      (emit/materialize {:format true})
      :string))

(defn formatted-ast [s]
  (-> (parse/ast s)
      (emit/materialize {:format true})))

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
