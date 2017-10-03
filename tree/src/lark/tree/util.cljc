(ns lark.tree.util
  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros])
     :clj
           (:require [net.cgrand.macrovich :as macros])))

(defmacro contains-identical-keyword?
  "Returns true if `x` is identical to any item in `coll` (expands to sequential `identical?` comparisons)"
  [coll x]
  `(or ~@(for [option coll]
           `((macros/case :clj ~'identical? :cljs ~'keyword-identical?)
              ~option ~x))))
