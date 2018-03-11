(ns lark.cards.parse
  (:require [lark.tree.core :as t]
            [lark.cards.editor :as editor :refer [CodeView]]
            [re-view.core :as v]
            [lark.tree.emit :as emit]
            [lark.tree.node :as node]
            [lark.tree.parse :as parse]))

(defn emit-node-with-decoration [{:keys [tag options children] :as node
                                  {:keys [invalid? error]} :options}]
  (let [possibly-children? (t/may-contain-children? node)]
    (if possibly-children?
      (let [[l r] (node/edges node)]
        [:span l (map emit-node-with-decoration children) r])
      (cond->> (emit/string node)
               invalid? (conj [:span.red])))))

(defn emit-node-structure
  [{:keys [tag options children] :as node}]
  (let [tag (cond->> (str tag)
                     (:invalid? options) (conj [:span.red])
                     (node/whitespace? node) (conj [:span.moon-gray]))]
    (if-not (node/may-contain-children? node)
      tag
      (-> [:span \[ [:span.b tag]]
          (into
           (when-not (empty? children)
             (interleave (repeat " ")
                         (mapv emit-node-structure children))))
          (conj \])))))

(v/defview show-parse
  {:key (comp first :view/children)
   :view/initial-state (fn [_ s] {:value s})}
  [{:keys [view/state]}]
  (let [value (:value @state)
        {:as node
         :keys [children invalid-nodes]} (parse/ast value)
        emitted (emit/string node)]
    [:div.bb.b--near-white.mv3.flex
     {:style {:white-space "pre-wrap"}}
     [:.pa2 {:style {:width "25%" :height 40}}
      (CodeView {:value value
                 :error-ranges invalid-nodes
                 :on-update #(swap! state assoc :value %)})]
     (into [:.pa2 {:style {:width "25%"}}]
           (map emit-node-with-decoration children))
     (into [:.pa2 {:style {:width "25%"}}
            (when-not (boolean (= emitted value))
              [:div
               [:div.red str "(not= emitted input) \nemitted: \n" (prn-str emitted) "\nvalue: \n" (prn-str value)]
               (str "\n" emitted "\n")])]
           (interpose " " (map emit-node-structure children)))]))

(v/defview cards []
  [:div.pa3
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
        (map show-parse))])