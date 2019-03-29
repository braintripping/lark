(ns lark.structure.operation.select
  (:require [lark.structure.pointer :as pointer]
            [lark.structure.delta :as delta]
            [lark.tree.core :as tree]
            [lark.structure.coords :as coords]))

(defn- move-path [loc pointer axis distance]
  (pointer/pointer loc
                   (update (pointer/resolve loc pointer)
                           (case axis :y 0
                                      :x 1)
                           +
                           distance)))

(defn- move-selection [loc [axis distance] {:as selection
                                            :keys [from to]}]
  (let [path (case axis
               :x (if (coords/point? selection)
                    (move-path loc from axis distance)
                    (if (pos? distance)
                      to
                      from))
               :y (let [path (or (if (pos? distance)
                                   to
                                   from)
                                 from)]
                    (move-path loc path :y distance)))]
    {:from path}))

(defn move [{:as state
             :keys [ast]} [axis distance]]
  (if (zero? distance)
    state
    (let [loc (tree/zip ast)]
      (update state :selections
              (fn [selections]
                (->> selections
                     (mapv #(move-selection loc [axis distance] %))
                     (distinct)))))))

(defn set-by-coords
  [state coord-spans]
  (let [loc (tree/zip (:ast state))]
    (assoc state :selections (mapv #(pointer/pointer-span loc %) coord-spans))))