(ns lark.cards.core
  (:require [chia.view :as v]
            [lark.cards.parse :as parse]
            [lark.structure.operation]
            [lark.structure.view :as structure-view]
            [chia.view.hooks :as hooks]))

(def pages {:structure [structure-view/view-samples]
            :parse [parse/cards]})

(v/defn index []
  (let [tab (hooks/use-atom :structure)]
    [:div (for [k (keys pages)]
            [:div {:style {:display "inline-block"}
                   :on-click #(reset! tab k)} (name k)])]
    (get pages @tab)))

(defn ^:dev/after-load render []
  (v/render-to-dom (index) (.getElementById js/document "app")))

(defonce _ (render))