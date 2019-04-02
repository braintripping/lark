(ns lark.cards.structure.play
  (:require [chia.view :as v]
            [chia.view.hooks :as hooks]
            [chia.reactive.atom :as ra]

            [lark.cards.cm :as cm]
            [lark.tree.core :as tree]
            [lark.tree.emit :as emit]
            [lark.keys :as keys]

            [lark.structure.operation :as op]
            [lark.structure.pointer :as pointer]

            [applied-science.js-interop :as j]))

(defn use-doc-listener [event f capture-phase]
  (let [f-ref (hooks/use-ref f)]
    (hooks/use-effect (fn []
                        (.addEventListener js/document event (j/get f-ref :current) capture-phase)
                        #(.removeEventListener js/document event (j/get f-ref :current) capture-phase)))))

(defonce last-key (atom nil))

(defn handle-keydown [editor-state e]
  (let [mods (keys/modifiers e)
        key (j/get e :key)
        ch? (and (= 1 (count key))
                 (empty? (disj mods :shift)))]

    ;; TODO
    ;; - restrict editing to valid operations
    ;; - always join terminals where possible
    ;; - constraint: the resulting ast of an operation is the same
    ;;   as the ast of the current operation parsed from scratch

    (cond (contains? keys/unsafe-combos (conj mods key))
          nil

          ch? (swap! editor-state op/operate :edit/replace key)
          :else
          (case key
            "Enter" (swap! editor-state op/operate :edit/replace \newline)
            "Backspace" (swap! editor-state op/operate :edit/replace "") ;; todo - cursor should delete backward
            "ArrowLeft" (swap! editor-state op/operate :selection/move [:x -1])
            "ArrowRight" (swap! editor-state op/operate :selection/move [:x 1])
            "ArrowUp" (swap! editor-state op/operate :selection/move [:y -1])
            "ArrowDown" (swap! editor-state op/operate :selection/move [:y 1])
            nil))


    (reset! last-key (if ch?
                       key
                       (str [key mods])))))

(v/defn editor []
  (let [editor-state (hooks/use-atom {:ast (tree/ast "(defn x [] (+ x 1))")})
        {:keys [ast
                selections]} @editor-state
        loc (tree/zip ast)]
    (use-doc-listener "keydown" (fn [e] (#'handle-keydown editor-state e)) true)
    [:div.pa4.f3.mono
     [cm/controlled {:value (emit/string ast)
                     :spans (mapv (partial pointer/resolve-span loc) selections)
                     :events {:beforeSelectionChange
                              (fn [cm data]
                                (when (= "*mouse" (j/get data :origin))
                                  (swap! editor-state op/operate
                                         :selection/set-by-coords
                                         (->> (j/get data :ranges)
                                              (mapv cm/span))))
                                false)}}]

     [:div.mv3 {:style {:color "#59deff"}} (ra/deref last-key)]
     [:div.mv3.f5 (prn-str ast (:children ast))]]))