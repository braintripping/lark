(ns lark.cards.core
  (:require [chia.view :as v]
            [lark.cards.parse :as parse]))

(defn render []
  (v/render-to-dom (parse/cards) (.getElementById js/document "app")))

(defonce _ (render))