(ns lark.tree.cursor
  (:require [lark.tree.nav :as nav]
            [fast-zip.core :as z]
            [lark.tree.node :as n]
            [lark.tree.range :as range]
            [lark.tree.util :as util]))

(defn path [loc pos]
  (let [loc (nav/navigate loc pos)
        node (z/node loc)
        [loc sticky data] (or #_(when (n/newline? node)
                                [loc :outer-right])
                              (when-let [inner-range (and (n/has-edges? node)
                                                          (range/inner-range node))]
                                (cond (range/pos= pos inner-range)
                                      [loc :inner-left]
                                      (range/pos= pos (range/end->start inner-range))
                                      [loc :inner-right]))
                              (when-let [adjacent-loc (->> [loc (z/left loc) (z/right loc)]
                                                           (remove nil?)
                                                           (filter (comp nav/path-loc-pred z/node))
                                                           (first))]
                                (let [adjacent-node (z/node adjacent-loc)]
                                  (cond (range/pos= pos adjacent-node)
                                        [adjacent-loc :outer-left]
                                        (range/pos= pos (range/end->start adjacent-node))
                                        [adjacent-loc :outer-right])))
                              (when (some-> (z/up loc)
                                            (z/node)
                                            (range/inner-range)
                                            (range/pos= pos))
                                [(z/up loc) :inner-left])
                              (when (and (not (n/whitespace? node))
                                         (n/terminal-node? node)
                                         (range/within? node pos))
                                [loc :terminal-offset [(- (:line pos) (:line node))
                                                       (- (:column pos) (:column node))]])
                              (when (and (nil? (z/right loc))
                                         (z/up loc))
                                [(z/up loc) :inner-right])

                              (when (some-> (z/left loc)
                                            (z/node)
                                            (range/end->start)
                                            (= pos))
                                [(z/left loc) :outer-right])

                              (when-let [loc (first (->> (nav/right-locs loc)
                                                         (take-while (comp (complement n/newline?) z/node))
                                                         (filter (comp (complement n/whitespace?) z/node))))]
                                [loc :outer-left])
                              (when-let [loc (or (some-> loc
                                                         (util/guard-> (comp n/newline? z/node)))
                                                 (first (->> (nav/left-locs loc)
                                                             (take-while (comp (complement n/newline?) z/node))
                                                             (filter (comp (complement n/whitespace?) z/node)))))]
                                [loc :outer-right])
                              (when-let [loc (->> [loc (z/up loc)]
                                                  (keep identity)
                                                  (filter (comp #(range/pos= node %)
                                                                range/inner-range
                                                                z/node))
                                                  (first))]
                                [loc :inner-left])
                              [loc :not-found])]
    (assert (nav/path-loc-pred (z/node loc)))
    [(nav/get-path loc)
     sticky
     data]))


(defn position [zipper [path sticky data]]
  (let [loc (nav/get-loc zipper path)
        node (z/node loc)]
    (assoc (case sticky
             :outer-right (range/bounds node :right)
             :outer-left (range/bounds node :left)
             :inner-right (-> (range/inner-range node)
                              (range/bounds :right))
             :inner-left (-> (range/inner-range node)
                             (range/bounds :left))
             :terminal-offset (let [[line column] data]
                                {:line (+ line (:line node))
                                 :column (+ column (:column node))}))
      :node node)))