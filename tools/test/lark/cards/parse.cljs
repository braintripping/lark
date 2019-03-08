(ns lark.cards.parse
  (:require [lark.tree.core :as t]
            [lark.cards.editor :as editor :refer [CodeView]]
            [chia.view :as v]
            [lark.tree.emit :as emit]
            [lark.tree.node :as node]
            [lark.tree.parse :as parse]))

(defn emit-node-structure
  [node]
  (let [tag (cond->> (str (.-tag node))
                     (:invalid? (.-options node)) (conj [:span.red])
                     (node/whitespace? node) (conj [:span.moon-gray]))]
    (if-not (node/may-contain-children? node)
      tag
      (-> [:span \[ [:span.b tag]]
          (into
           (when-not (empty? (.-children node))
             (interleave (repeat " ")
                         (mapv emit-node-structure (.-children node)))))
          (conj \])))))

(vlegacy/defview show-parse
  {:key (comp first :view/children identity)
   :view/initial-state (fn [_ _ s] {:value s})}
  [{:keys [view/state]}]
  (let [value (:value @state)
        node (emit/materialize (parse/ast value))
        emitted (:string node)]
    [:div.bb.b--near-white.pa3
     {:style {:white-space "pre-wrap"}}
     [:div.ph1
      (CodeView {:value value
                 :error-ranges (:invalid-nodes node)
                 :on-update #(swap! state assoc :value %)})]
     [:div.pa1
      (when-not (boolean (= emitted value))
        [:div
         [:div.red "(not= emitted input) \nemitted: \n" (prn-str emitted) "\nvalue: \n" (prn-str value)]
         (str "\n" emitted "\n")])

      (interpose " " (map emit-node-structure (.-children node)))]

     ]))

(vlegacy/defview cards []
  [:div
   {:style {:font-family "Menlo, Monaco, \"Courier New\", monospace"
            :font-size 12}}
   (->> ["\\"
         "\\tab"
         "\\ta"
         "#?("
         "[']"
         "[^]"
         "[~]"
         "[^{}]"
         "[^{} a-sym]"
         "#"
         "#\""
         "#\"abc "
         "#\"\""
         "#\"[a-b]+\""
         ":normal-kw"
         "::lookup-kw"
         ":invalid-kw/"
         ":"
         "::"
         "::/"
         "::a/"
         "symbol"
         "invalid-sym/"
         "["
         "]"
         "[)]"
         "(()"
         "#{(}"
         "[("
         "[(])"
         "[1 2 (3 4 ]"
         "[1 2 (3 (4 ]"
         "[1 2 )3 )4 ]"
         "1"
         "prn"
         "\"hello\""
         ""
         ":hello"
         ":a/b"
         "::wha"
         "#(+)"
         "[1 2 3]\n3 4  5, 9"
         "^:dynamic *thing*"
         "(f x)"
         "#{1}"
         "#^:a {}"
         "#'a"
         "@a"
         "#_()"
         "'a"
         "`a"
         "~a"
         "#?(:cljs (+ 1 1))"
         "#?@(:cljs (+ 1 1))"
         "#?(:cljs 1 :cljs 2)"
         "#?(:clj 1 :cljs 2)"
         "#?(:clj 1)"]
        (map-indexed show-parse))])
