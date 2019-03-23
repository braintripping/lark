(ns lark.cards.structure.position
  (:require [lark.tree.range :as range]
            [fast-zip.core :as z]
            [lark.tree.nav :as nav]
            [lark.cards.structure.loc :as loc]
            [lark.tree.node :as n]))

;;;;;;;;;;;;;;;;
;;
;; Nodes and Positions

(defn within-left-edge?
  "Position is within left edge, or at left inner boundary of empty node."
  [node pos]
  (let [inner-left (range/inner-left node)]
    (or (range/lt pos inner-left)
        (and (not (seq (.-children node)))
             (range/pos= pos inner-left)))))

(defn right-edge? [node pos]
  (and (not (= :base (.-tag node)))
       (range/pos= (range/bounds node :right) pos)))

(defn clamp-pos
  "If `pos` is outside bounds of `node`, constrains to fit."
  [node pos]
  (case (.-tag node)
    ;; pos is outside of base node
    :base (cond (range/lt pos node) (range/bounds node :left)
                (range/gt pos (range/bounds node :right)) (range/bounds node :right)
                :else pos)
    ;; pos has been adjusted within a newline node, needs to move to correct line
    :newline (cond (neg? (:column pos)) (range/bounds node :left)
                   (and (= (:line pos) (:line node))
                        (> (:column pos) (:column node))) {:line (inc (:line node))
                                                           :column (dec (- (:column pos) (:column node)))}
                   :else pos)
    pos))

;;;;;;;;;;;;;;;;
;;
;; Paths and Positions

(defn pos-offset [{node-line :line
                   node-col :column} {pos-line :line
                                      pos-col :column}]
  [(- pos-line node-line)
   (if (= node-line pos-line)
     (- pos-col node-col)
     pos-col)])

(defn pos->cursor-path*
  "Returns a cursor-path for a position within a loc.

  position:    {... :line, :column}
  cursor-path: [loc-path offset meta]
  offset: [line column from-end?]"
  [loc pos]
  (let [node (.-node loc)
        pos (clamp-pos node pos)]
    (cond (or (n/terminal? node)
              (within-left-edge? node pos)) [(loc/path loc)
                                             (pos-offset node pos)]
          (right-edge? node pos) [(loc/path loc)
                                  (conj (pos-offset (range/bounds node :right) pos) true)]
          :else
          (recur (->> (nav/child-locs loc)
                      (take-while (comp #(range/lt= (range/bounds % :left) pos)
                                        z/node))
                      #_(sequence (comp (drop-while #(range/lt (range/bounds (z/node %) :right)
                                                               pos))
                                        (take-while #(range/lt= pos (range/bounds (z/node %) :right)))))
                      (last)) pos))))

(defn pointer [loc pos]
  (pos->cursor-path* loc (range/bounds pos :left)))

(defn resolve-offset [[line-offset column-offset from-end?] node]
  (let [{node-line :line
         node-col :column} (cond-> node
                                   from-end? (range/bounds :right))]
    {:line (+ line-offset node-line)
     :column (if (zero? line-offset)
               (+ node-col column-offset)
               column-offset)}))

(defn pointer->pos [root-loc [path offset]]
  (let [loc (loc/get-loc root-loc path)
        node (some-> loc z/node)]
    (resolve-offset offset node)))

(defn range->selection [loc range]
  (if (:end-line range)
    {:from (pointer loc range)
     :to (pointer loc (range/bounds range :right))}
    {:from (pointer loc range)}))

(defn normalize-range [range]
  (if (and (:end-line range)
           (= (:line range) (:end-line range))
           (= (:column range) (:column range)))
    (select-keys range [:line :column])
    range))