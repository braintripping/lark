(ns lark.cards.structure.core
  (:require lark.cards.structure.coords
            [lark.cards.structure.delta :as delta]
            lark.cards.structure.loc
            lark.cards.structure.operation
            lark.cards.structure.pointer
            [lark.cards.structure.position :as pos]
            lark.cards.structure.serialize-selections
            lark.cards.structure.string

            [cljs.spec.alpha :as s]
            [expound.alpha :as expound]
            [cljs.repl :as repl]

            [cljs.spec.test.alpha :as st]

            [applied-science.js-interop :as j]
            [chia.util :as u]))


(set! s/*explain-out* expound/printer)

(def devtools-error-formatter
  "Uses cljs.repl utilities to format ExceptionInfo objects in Chrome devtools console."
  #js{:header
      (fn [object config]
        (when (instance? ExceptionInfo object)
          (let [message (some->> (repl/error->str object)
                                 (re-find #"[^\n]+"))]
            #js["span" message])))
      :hasBody (constantly true)
      :body (fn [object config]
              #js["div" (repl/error->str object)])})
(defonce _
         (some-> js/window.devtoolsFormatters
                 (.unshift devtools-error-formatter)))


(defn add-formatter! []
  (some-> js/window.devtoolsFormatters
          (.unshift devtools-error-formatter)))

(defn ^:dev/after-load instrument []
  (st/instrument '[lark.cards.structure.coords
                   lark.cards.structure.delta
                   lark.cards.structure.loc
                   lark.cards.structure.operation
                   lark.cards.structure.path
                   lark.cards.structure.pointer
                   lark.cards.structure.position
                   lark.cards.structure.serialize-selections
                   lark.cards.structure.string]))



(defonce _
         (do (add-formatter!)
             (instrument)))

