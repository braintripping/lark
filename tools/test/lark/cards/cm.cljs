(ns lark.cards.cm
  (:require ["codemirror" :as CM]
            ["codemirror/mode/clojure/clojure"]
            ["codemirror/mode/markdown/markdown"]

            [chia.view :as v]
            [chia.view.hooks :as hooks]
            [applied-science.js-interop :as j]
            [chia.util :as u]
            [lark.cards.utils :as cu]
            [chia.jss :as jss]
            [goog.dom :as gdom]))

(v/defn editor [{:as props
                 :keys [key
                        value
                        default-value
                        options

                        mode
                        ranges]
                 :or {mode "clojure"}}]
  (let [cm-ref (hooks/use-ref)
        {:keys [sel
                sel-start
                sel-end
                cursor]} (jss/classes! {:sel {:background "rgba(0,255,255,0.2)"}
                                        :sel-start {:border-left "2px solid rgba(0,210,210,0.9)"}
                                        :sel-end {:border-right "2px solid rgba(0,210,210,0.9)"}
                                        :cursor {:border-left "2px solid rgba(0,210,210,0.9)"
                                                 :width 0
                                                 :display "inline-block"}})
        element-ref (hooks/use-dom-ref
                     (fn [element]
                       (let [cm (CM element (clj->js
                                             (merge options
                                                    {:value (:value props default-value)
                                                     :mode mode})))]
                         (j/assoc! cm-ref :current cm)
                         (doseq [[k f] (u/select-ns props "on")]
                           (j/call cm :on (name k) f)))))]
    (hooks/use-effect
     (fn []
       (let [cm (j/get cm-ref :current)]
         (doto cm
           (cond-> value
                   (.setValue value))
           #_(cond-> selections
                     (.setSelections (to-array (mapv cu/->cm-range selections)))))

         (doseq [mark (j/call cm :getAllMarks)]
           (j/call mark :clear))

         (doseq [{:as range
                  :keys [line column end-line end-column]} ranges]
           (if end-line
             (j/call cm :markText
                        #js {:line line :ch column}
                        #js {:line end-line :ch end-column}
                        #js{:className sel
                            :startStyle sel-start
                            :endStyle sel-end})
             (j/call cm :setBookmark
                        #js {:line line :ch column}
                        #js {:widget (doto (gdom/createElement "DIV")
                                       (gdom/setProperties #js{:class cursor
                                                               :innerHTML " "}))})))

         )))
    [:div {:ref element-ref}]))