(ns lark.cards.structure.operation
  (:require [lark.cards.structure.core :as st]
            [lark.tree.core :as tree]
            [fast-zip.core :as z]
            [lark.tree.node :as n]
            [lark.tree.emit :as emit]
            [cljs.pprint :as pp]
            [lark.tree.parse :as parse]))

(defn cursor? [[from to]]
  (nil? to))

(defn replace-selection [ast [from-path to-path] s]
  [ast {:selections [[from-path to-path]]}])

(defn offset-selections [{:as state
                          :keys [selections]}]
  )

(defn insert-text [ast [path cursor-offset] text]
  (let [loc (-> (tree/ast-zip ast)
                (st/get-in-loc path))

        text-before (emit/string (z/node loc))
        text-after (st/insert-at-coords text-before
                                        cursor-offset
                                        text)

        ast (-> text-after
                (parse/ast)

                (->> (z/replace loc))
                (z/root))

        text-offset (st/string-offset text)
        cursor-offset (st/add-offsets cursor-offset text-offset)]
    [ast {:offsets {path text-offset}
          :selections [[[path (st/add-offsets cursor-offset text-offset)]]]}]))

(defn apply-to-selections [f {:as state
                              :keys [selections]}]
  (-> (reduce (fn [{:as state
                    :keys [ast
                           accrued-offsets]} selection]
                (let [_ (prn :sel-before selection)
                      selection (->> selection
                                     (mapv (fn [[path offset]]
                                             [path (st/add-offsets (get accrued-offsets path) offset)])))
                      _ (prn :sel-after selection)
                      [ast {:keys [offsets
                                   selections]}] (f ast selection)]
                  (-> state
                      (assoc :ast ast)
                      (update :accrued-offsets merge-with (partial st/add-offsets offsets))
                      (update :selections into selections)
                      (dissoc :offsets))))
              (assoc state :selections []
                           :accrued-offsets {})
              selections)
      (update :ast emit/materialize)))

(defmethod st/operate :edit/insert-text
  [_ {:as state
      :keys [ast
             selections]} [s]]
  (apply-to-selections
   (fn [ast selection]
     (if (cursor? selection)
       (insert-text ast (first selection) s)
       (replace-selection ast selection s))) state))
=
(defmethod st/operate :identity
  [_ state]
  state)


(defn- move-path [loc path axis distance]
  (->> (update (st/path->pos loc path)
               (case axis :x :column
                          :y :line)
               +
               distance)
       (st/pos->path loc)))

(defn- move-selection [loc [axis distance] [from to]]
  (let [cursor? (nil? to)]
    (case axis
      :x (if cursor?
           (move-path loc from axis distance)
           (if (pos? distance) to from))
      :y (let [path (or (if (pos? distance) to from)
                        from)]
           (move-path loc path :y distance)))))

(defmethod st/operate :cursor/move
  [_ {:as state
      :keys [ast
             selections]} [axis distance]]
  (if (zero? distance)
    state
    (let [loc (tree/ast-zip ast)
          selections (into []
                           (comp (map (partial move-selection loc [axis distance]))
                                 (map vector)
                                 (distinct))
                           selections)]
      (assoc state :selections selections))))

#_(defmethod operate :cursor/hop
    [_ {:as state
        :keys [ast
               selections]} direction]
    (let [loc (tree/ast-zip ast)]
      (assoc state :selections
                   (->> selections
                        (reduce (fn [out [from to :as selection]]
                                  (conj out
                                        (case axis
                                          :x (if to
                                               (if (pos? distance)
                                                 [to]
                                                 [from])
                                               [(-> (path->pos loc from)
                                                    (update :column + distance)
                                                    (->> (pos->path loc)))])
                                          :y state))) [])
                        (distinct)))))

