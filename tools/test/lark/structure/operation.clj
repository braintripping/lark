(ns lark.structure.operation)

(defmacro defop [k f]
  `(swap! ~'lark.structure.operation/registry assoc ~k ~f))