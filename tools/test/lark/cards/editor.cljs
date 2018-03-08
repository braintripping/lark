(ns lark.cards.editor
  (:refer-clojure :exclude [require])
  (:require [re-view.core :as v]
            [re-db.d :as d]
            [clojure.set :as set]
            [lark.editors.codemirror :as cm]
            ["codemirror" :as CM]))

(def monaco nil)

(defn require [scripts]
  (js/require (clj->js scripts)
              #(do
                 (d/transact! [[:db/update-attr :scripts :loaded (fnil into #{}) scripts]])
                 (set! monaco %))))

(defn init []
  (.config js/require
           #js {:paths #js {:vs "/js/monaco/dev/vs"}})
  (require ["vs/editor/editor.main"]))

(init)

(v/defview with-scripts
  [{:keys [scripts
           loader]} the-view]
  (if (set/superset? (d/get :scripts :loaded)
                     (set scripts))
    the-view
    loader))

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

(defn get-height [editor line-count]
  (let [config (.. editor (getConfiguration))
        line-count (max 1 (.. editor (getModel) (getLineCount)))
        line-height (.-lineHeight config)]
    (-> (* line-height
           line-count)
        #_(> (.getScrollWidth editor)
             (.. config -layoutInfo -contentWidth))
        (+ (.. config -layoutInfo -horizontalScrollbarHeight)))))

(defn set-height! [M]
  (let [line-count (.. M (getModel) (getLineCount))]
    (when (not= line-count (.-prevLineCount M))
      (set! (.-prevLineCount M) line-count)
      (.layout M #js {:height (get-height M line-count)
                      :width (.-offsetWidth (.-domElement M))}))))

(v/defview editor*
  {:view/did-mount
   (fn [{:as this
         :keys [value
                on-value
                on-change-cursor
                on-focus
                before-change]}]
     (let [M (.. monaco -editor (create (v/dom-node this)
                                        #js {:value value
                                             :theme "vs-light"
                                             :language "clojure"
                                             :scrollBeyondLastLine false
                                             :overviewRulerBorder false
                                             :hideCursorInOverviewRuler true
                                             :autoClosingBrackets false
                                             :matchBrackets false
                                             :codeLens false
                                             :minimap #js {:enabled false}
                                             :renderLineHighlight "none"
                                             :lineNumbers false}))]
       (set! (.-monaco this) M)
       ;(.onDidChangeCursorPosition M js/console.log)
       (.onDidChangeModelContent M (partial set-height! M))
       (when on-value
         (.onDidChangeModelContent M #(on-value (.getValue M))))
       ;(.onDidChangeModel M js/console.log)
       (set-height! M)

       (aset js/window "e" M)

       ))}
  [this]
  [:div])

(defn editor [props]
  (with-scripts
   {:scripts #{"vs/editor/editor.main"}
    :loader "Loading..."}
   (editor* props)))

;; decoration
;; IModelDecorationOptions
;; stickiness?: monaco.TrackedRangeStickiness.<>
;; hoverMessage
;; isWholeLine
;; className
;;
;; IModelDeltaDecoration
;; range, options


(v/defview CodeView
  {:view/spec {:props {:event/mousedown :Function
                       :event/keydown :Function
                       :on-ast :Function
                       :keymap :Map}}
   :view/did-mount (fn [{:keys [default-value
                                value
                                on-update
                                view/state
                                error-ranges]
                         :as this}]
                     (let [dom-node (v/dom-node this)
                           cm-editor (CM dom-node
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
                                                   :configureMouse (fn [cm repeat e]
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

                       (when on-update
                         (.on cm-editor "change" #(on-update (.getValue %1))))))
   :get-editor (fn [this]
                 (:cm-editor @(:view/state this)))
   :reset-value (fn [{:keys [default-value value view/state]}]
                  (.setValueAndRefresh (:cm-editor @state) (or value default-value)))
   :view/will-receive-props (fn [{:keys [value source-id error-ranges]
                                  {prev-source-id :source-id} :view/prev-props
                                  :as this}]
                              (let [cm-editor (:cm-editor @(:view/state this))]
                                (if (or (not= source-id prev-source-id)
                                        (not= value (.getValue cm-editor)))
                                  (.resetValue this)
                                  nil)
                                (some->> error-ranges
                                         (cm/highlight-parse-errors! cm-editor))))
   :view/should-update (fn [_] false)}
  [{:keys [view/state view/props] :as this}]
  [:.cursor-text])