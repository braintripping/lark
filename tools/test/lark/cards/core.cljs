(ns lark.cards.core
  (:require [chia.view :as v]
            [lark.structure.operation]
            [lark.cards.parse :as parse]
            [lark.cards.structure.view-samples :as samples]
            [lark.cards.structure.play :as play]
            [chia.view.hooks :as hooks]))

(v/defn structure []
  [:<>
   [samples/view]
   [play/editor]])

(def pages {:structure [structure]
            :parse [parse/cards]})

(v/defn index []
  (let [tab (hooks/use-atom :structure)]
    [:div
     #_(for [k (keys pages)]
         [:div {:style {:display "inline-block"}
                :on-click #(reset! tab k)} (name k)])

     (get pages @tab)]))

(defn ^:dev/after-load render []
  (v/render-to-dom (index) (.getElementById js/document "app")))

(defonce _ (render))