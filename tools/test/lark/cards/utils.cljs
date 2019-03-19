(ns lark.cards.utils
  (:require [applied-science.js-interop :as j]
            [lark.tree.node :as node]
            [lark.tree.range :as range]
            [lark.tree.node :as n]
            [fast-zip.core :as z]
            [lark.tree.nav :as nav]
            [chia.jss :as jss]
            [chia.view :as v]))

(defn navigate
  "Navigate to a position within a zipper (returns loc) or ast (returns node)."
  [ast pos]
  (assert pos)
  (if (= (type ast) z/ZipperLocation)
    (let [loc ast
          node (.-node loc)
          children (.-children node)
          found (when (range/within? node pos)
                  (if
                   (or (n/terminal? node)
                       (empty? children))
                    loc
                    (or
                     (some-> (filter #(range/within? % pos) (nav/child-locs loc))
                             first
                             (navigate pos))
                     loc)))]
      (if (let [found-node (some-> found (.-node))]
            (and (= (get pos :line) (get found-node :end-line))
                 (= (get pos :column) (get found-node :end-column))))
        (or (z/right found) found)
        found))
    (when (range/within? ast pos)
      (if
       (or (n/terminal? ast)
           (empty? (get ast :children)))
        ast
        (or (some-> (filter #(range/within? % pos) (get ast :children))
                    first
                    (navigate pos))
            (when-not (= :base (get ast :tag))
              ast))))))

(defn lt? [l1 c1 l2 c2]
  (or (< l1 l2)
      (and (= l1 l2)
           (< c1 c2))))

(defn _< [{line-1 :line col-1 :column}
          {line-2 :line col-2 :column}]
  (or (< line-1 line-2)
      (and (= line-1 line-2)
           (< col-1 col-2))))

(defn _<= [{line-1 :line col-1 :column}
           {line-2 :line col-2 :column}]
  (or (< line-1 line-2)
      (and (= line-1 line-2)
           (<= col-1 col-2))))

(defn parse-cm-range [range]
  (let [anchor* (j/get range :anchor)
        head* (j/get range :head)
        al (j/get anchor* :line)
        ac (j/get anchor* :ch)
        hl (j/get head* :line)
        hc (j/get head* :ch)
        forward? (lt? al ac hl hc)]
    {:line (if forward? al hl)
     :column (if forward? ac hc)
     :end-line (if forward? hl al)
     :end-column (if forward? hc ac)
     :point? (and (= al hl)
                  (= ac hc))}))

(defn ->cm-range [{:keys [line column
                          end-line end-column]}]
  #js {:anchor #js {:line line :ch column}
       :head #js {:line end-line :ch end-column}})

(defn offset [p1 p2]
  [(- (:line p2) (:line p1))
   (- (:column p2) (:column p1))])

(defn emit-node-structure
  [node]
  (let [tag (.-tag node)
        format-token #(cond->> (str %)
                               (:invalid? (.-options node)) (conj [:span.red])
                               (node/whitespace? node) (conj [:span.moon-gray]))]
    (if-not (node/may-contain-children? node)
      (format-token tag)
      (-> [:span \( [:span.b (name tag)]]
          (into
           (when-not (empty? (.-children node))
             (interleave (repeat " ")
                         (mapv emit-node-structure (.-children node)))))
          (conj \))))))

(defn vector-splice
  "Splice items into vector at index, replacing n items"
  [the-vector from-i to-i items]
  (-> (subvec the-vector 0 from-i)
      (into items)
      (into (subvec the-vector (inc to-i) (count the-vector)))))


(comment
 (= (plant-ranges "abc" [{:line 0 :column 1
                          :end-line 0 :end-column 2}])
    "a<b>c")
 (= (plant-ranges "abc" [{:line 0 :column 1}])
    "a|bc"))

(v/defn show-selections [selection-paths]
  (->> selection-paths
       (map (fn [sel]
              (->> sel
                   (map prn-str)
                   (interpose [:div.moon-gray "-"]))))
       (apply concat)))
