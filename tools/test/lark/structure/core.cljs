(ns lark.structure.core
  (:require lark.structure.coords
            lark.structure.loc
            lark.structure.operation
            lark.structure.pointer
            lark.structure.string

            [cljs.spec.alpha :as s]
            [expound.alpha :as expound]
            [cljs.spec.test.alpha :as st]

            [chia.util.dev-errors :as dev-errors]))


(set! s/*explain-out* expound/printer)

(defn ^:dev/after-load instrument []
  (st/instrument '[lark.structure.operation
                   lark.structure.operation.edit
                   lark.structure.operation.select
                   lark.structure.operation.impl.replace
                   lark.structure.coords
                   lark.structure.delta
                   lark.structure.loc
                   lark.structure.path
                   lark.structure.pointer
                   lark.structure.string]))

(defonce _
         (do (dev-errors/install-formatter!)
             (instrument)))

