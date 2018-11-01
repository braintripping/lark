(ns lark.cards.editor
  (:refer-clojure :exclude [require])
  (:require [chia.view :as v]
            [lark.editor :as editor]
            [lark.editors.codemirror :as cm]
            ["codemirror" :as CM]
            [chia.util.js-interop :as j]))

#_(def options
    {:theme "maria-light"
     :lineNumbers false
     :lineWrapping true
     :cursorScrollMargin 40
     :mode "clojure"
     :keyMap "default"
     :styleSelectedText true
     :flattenSpans true
     :configureMouse (fn [cm repeat e]
                       #js {:moveOnDrag (if (.-shiftKey e)
                                          false
                                          true)})})

(defn reset-value [^js editor next-value]
  (.setValueAndRefresh editor next-value #_(or value default-value)))

(v/defview CodeView
  {:spec/props {:event/mousedown :Function
                :event/keydown :Function
                :on-ast :Function
                :keymap :Map}
   :view/did-mount (fn [{:keys [default-value
                                value
                                on-update
                                view/state
                                error-ranges]
                         :as this}]
                     (let [dom-node (v/dom-node this)
                           ^js cm-editor (CM dom-node
                                             (clj->js {:value (str (or value default-value))
                                                       :theme "maria-light"
                                                       :lineNumbers false
                                                       :lineWrapping true
                                                       :cursorScrollMargin 40
                                                       :mode "clojure"
                                                       :keyMap "default"
                                                       :styleSelectedText true
                                                       :magicBrackets true
                                                       :magicEdit true
                                                       :flattenSpans true
                                                       :configureMouse (fn [cm repeat ^js e]
                                                                         #js {:moveOnDrag (if (.-shiftKey e)
                                                                                            false
                                                                                            true)})}))]
                       (set! (.-view cm-editor) this)

                       (swap! cm-editor assoc :view this)

                       (set! (.-setValueAndRefresh cm-editor)
                             #(do (cm/set-preserve-cursor! cm-editor %)
                                  (.refresh cm-editor)))

                       (swap! state assoc :cm-editor cm-editor)

                       (some->> error-ranges
                                (cm/highlight-parse-errors! cm-editor))

                       #_(.setValue cm-editor (or value default-value ""))

                       (when on-update
                         (.on cm-editor "change" #(on-update (doto (.getValue %1) prn))))))
   :view/did-update (fn [{:keys [value
                                 default-value
                                 source-id
                                 error-ranges]
                          {prev-source-id :source-id} :view/prev-props
                          :as this}]
                      (let [cm-editor (editor/get-editor this)
                            value (or value default-value)]
                        (if (or (not= source-id prev-source-id)
                                (not= value (.getValue cm-editor)))
                          (reset-value cm-editor value)
                          nil)
                        (some->> error-ranges
                                 (cm/highlight-parse-errors! cm-editor))))
   :view/should-update (fn [_] false)}
  [{:keys [view/props]
    :as this}]
  [:.cursor-text])

(v/extend-view CodeView
  editor/IEditor
  (get-editor [this] (-> this
                         :view/state
                         (deref)
                         :cm-editor)))
