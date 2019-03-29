(ns lark.cards.structure.play
  (:require [chia.view :as v]
            [chia.view.hooks :as hooks]
            [chia.reactive.atom :as ra]

            [lark.cards.cm :as cm]
            [lark.tree.core :as tree]
            [lark.tree.emit :as emit]

            [lark.structure.operation :as operation]
            [lark.structure.pointer :as pointer]

            [applied-science.js-interop :as j]))

(defonce last-key (atom nil))

(defn before-input [e]
  (reset! last-key (j/get e :key)))

(defn use-doc-listener [event f capture-phase]
  (let [f-ref (hooks/use-ref f)]
    (hooks/use-effect (fn []
                        (.addEventListener js/document event (j/get f-ref :current) capture-phase)
                        #(.removeEventListener js/document event (j/get f-ref :current) capture-phase)))))

(v/defn editor []
  (let [[{:as editor-state
          :keys [ast
                 selections]} set-state!] (hooks/-use-state {:ast (tree/ast "(defn x [] (+ x 1))")})
        loc (tree/zip ast)]
    (use-doc-listener "keypress" (fn [e] (j/call e :preventDefault) (#'before-input e)) true)
    [:div.pa4.f3.mono
     [cm/controlled {:value (emit/string ast)
                     :spans (mapv (partial pointer/resolve-span loc) selections)
                     :events {:beforeSelectionChange
                              (fn [cm data]
                                (when (= "*mouse" (j/get data :origin))
                                  (->> (j/get data :ranges)
                                       (mapv cm/span)
                                       (operation/operate :selection/set-by-coords editor-state)
                                       (set-state!)))
                                false)}}]

     [:div.mv3 {:style {:color "#59deff"}} (ra/deref last-key)]]))