(ns lark.tree.util
  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros])
     :clj
           (:require [net.cgrand.macrovich :as macros])))

;; Successive `identical?` comparisons are _significantly_ faster than idiomatic alternatives such as `(contains? #{:k1 :k2} the-keyword)`,
;; results in a 2x overall speedup in parse/ast.

(defmacro contains-identical-keyword?
  "Returns true if `x` is identical to any item in `coll` (expands to sequential `keyword-identical?` (cljs) or `identical?` (clj) comparisons)."
  [coll x]
  `(or ~@(for [option coll]
           `((macros/case :clj ~'identical? :cljs ~'keyword-identical?)
              ~option ~x))))

(defmacro contains-identical?
  "Returns true if `x` is identical to any item in `coll` (expands to sequential `identical?` comparisons)."
  [coll x]
  `(or ~@(for [option coll]
           `(identical? ~option ~x))))
