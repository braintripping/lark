(ns lark.structure.operation.impl)

(defmulti -operate identity)

(defmethod -operate :default
  [op state]
  (js/console.warn (str "Operation not implemented: " op))
  state)
