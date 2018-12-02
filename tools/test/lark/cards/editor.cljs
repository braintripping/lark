(ns lark.cards.editor
  (:refer-clojure :exclude [require])
  (:require [chia.view :as v]
            [lark.editor :as editor]
            [lark.editors.codemirror :as cm]
            ["codemirror" :as CM]
            [chia.util.js-interop :as j]
            [clojure.spec.alpha :as s]))

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

(s/def :event/mousedown fn?)
(s/def :event/keydown fn?)
(s/def ::on-ast fn?)
(s/def ::keymap map?)
(s/def ::error-ranges (s/nilable (s/coll-of #(:line %))))

(defn reset-value [^js editor next-value]
  (cm/set-value-and-refresh! editor next-value #_(or value default-value)))

(v/defview CodeView
  {:spec/props (s/keys
                :opt [:event/mousedown
                      :event/keydown]
                :opt-un [::on-ast
                         ::keymap
                         ::error-ranges])
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
                                                       :viewportMargin js/Infinity
                                                       :configureMouse (fn [cm repeat ^js e]
                                                                         #js {:moveOnDrag (if (.-shiftKey e)
                                                                                            false
                                                                                            true)})}))]
                       (set! (.-view cm-editor) this)

                       (swap! cm-editor assoc :view this)

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
