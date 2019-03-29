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
            [goog.dom :as gdom]
            [lark.structure.coords :as coords]))

(defn coords [pos]
  [(j/get pos :line)
   (j/get pos :ch)])

(defn span [range]
  {:from (coords (j/call range :from))
   :to (coords (j/call range :to))})

(def classes (delay
              (let [edge (fn [side]
                           {:content "' '"
                            :position "absolute"
                            :top 0
                            side 0
                            :bottom 0
                            (keyword (str "margin-" (name side))) -1
                            (keyword (str "border-" (name side))) "2px solid rgba(0,210,210,0.9)"})]
                (jss/classes! {:sel {:background "rgba(0,255,255,0.2)"}
                               :sel-start {:position "relative"
                                           "&::after" (edge :left)}
                               :sel-end {:position "relative"
                                         "&::after" (edge :right)}
                               :cursor {:position "relative"
                                        :display "inline-block"
                                        :width 0
                                        "&::after" (edge :left)}}))))

(defn use-codemirror [{:keys [mode options events value]
                       :or {mode "clojure"}}]
  (let [cm-ref (hooks/use-ref)
        element-ref (hooks/use-dom-ref
                     (fn [element]
                       (let [cm (CM element (clj->js (merge options
                                                            {:mode mode
                                                             :value value})))]
                         (j/assoc! cm-ref :current cm)
                         (doseq [[k f] events]
                           (j/call cm :on (name k) f)))))]
    [element-ref cm-ref]))

(defn mark-span! [cm {:as span
                      [line column :as from] :from
                      [end-line end-column :as to] :to}]
  (if (coords/point? span)
    (j/call cm :setBookmark
               (j/obj :line line :ch column)
               (j/obj :widget (doto (gdom/createElement "DIV")
                                (gdom/setProperties #js{:class (:cursor @classes)
                                                        :innerHTML " "}))))
    (j/call cm :markText
               (j/obj :line line :ch column)
               (j/obj :line end-line :ch end-column)
               (j/obj :className (:sel @classes)
                      :startStyle (:sel-start @classes)
                      :endStyle (:sel-end @classes)))))

(defn apply-marks! [cm spans]

  (doseq [mark (j/call cm :getAllMarks)]
    (j/call mark :clear))

  (doseq [span spans]
    (mark-span! cm span)))

(v/defn controlled [{:as props
                     :keys [value
                            default-value
                            options
                            events
                            mode
                            spans]
                     :or {mode "clojure"}}]
  (let [[element-ref cm-ref] (use-codemirror {:value (:value props default-value)
                                              :events events
                                              :mode mode
                                              :options options})]
    (hooks/use-effect
     (fn []
       (-> (j/get cm-ref :current)
           (doto
             (cond-> value (j/call :setValue value))
             (apply-marks! spans)))))

    [:div.mono {:ref element-ref}]))
