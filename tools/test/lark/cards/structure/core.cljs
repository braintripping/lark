(ns lark.cards.structure.core
  (:require [lark.tree.range :as range]
            [fast-zip.core :as z]
            [lark.tree.nav :as nav]
            [lark.tree.node :as n]
            [lark.tree.core :as tree]
            [lark.tree.emit :as emit]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;
;;
;; Zipper locs

(defn get-in-loc
  "Returns `loc` at `path`"
  [loc path]
  (loop [segments path
         loc loc]
    (if (or (not loc)
            (empty? segments))
      loc
      (recur (rest segments)
             (->> (iterate z/right (z/down loc))
                  (take-while identity)
                  (drop (first segments))
                  (first))))))

(defn path-to-root
  "Returns path to `loc` from root"
  [loc]
  (loop [loc loc
         out ()]
    (if-not loc
      (drop 1 out)
      (recur (z/up loc)
             (cons (->> (z/lefts loc)
                        (count)) out)))))

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

(defn wrap-path [path offset tag]
  (with-meta path {::offset offset
                   ::tag tag}))

(defn pos->path*
  "Returns a cursor-path for a position within a loc.

  position:    {... :line, :column}
  cursor-path: [loc-path offset meta]
  offset: [line column from-end?]"
  [loc pos]
  (let [node (.-node loc)
        tag (.-tag node)
        pos (clamp-pos node pos)]
    (cond (or (n/terminal? node)
              (within-left-edge? node pos)) [(path-to-root loc) (pos-offset node pos) tag]
          (right-edge? node pos) [(path-to-root loc)
                                  (conj (pos-offset (range/bounds node :right) pos) true)
                                  tag]
          :else
          (recur (->> (nav/child-locs loc)
                      (take-while (comp #(range/lt= (range/bounds % :left) pos)
                                        z/node))
                      #_(sequence (comp (drop-while #(range/lt (range/bounds (z/node %) :right)
                                                               pos))
                                        (take-while #(range/lt= pos (range/bounds (z/node %) :right)))))
                      (last)) pos))))

(defn pos->path [loc pos]
  (pos->path* loc (range/bounds pos :left)))

(defn resolve-offset [[line-offset column-offset from-end?] node]
  (let [{node-line :line
         node-col :column} (cond-> node
                                   from-end? (range/bounds :right))]
    {:line (+ line-offset node-line)
     :column (if (zero? line-offset)
               (+ node-col column-offset)
               column-offset)}))

(defn path->pos [root-loc [path offset]]
  (let [loc (get-in-loc root-loc path)
        node (some-> loc z/node)]
    (resolve-offset offset node)))

;;;;;;;;;;;;;;;;
;;
;; Selections and Ranges
;;
;; - a selection is a 2-tuple of paths
;; - a range has :line, :column, ?:end-line, ?:end-column keys
;; - a cursor does not have an :end-line (?)

(defn selection->range [loc [from-path to-path]]
  (merge
   (range/bounds (path->pos loc from-path) :left)
   (when to-path
     (range/->end (path->pos loc to-path)))))

(defn range->selection [loc range]
  (if (:end-line range)
    [(pos->path loc range)
     (pos->path loc (range/bounds range :right))]
    [(pos->path loc range)]))

;;;;;;;;;;;;;;;;
;;
;; Operations

(defmulti operate identity)

(defmethod operate :default
  [op state]
  (js/console.warn (str "Operation not implemented: " op))
  state)

;;;;;;;;;;;;;;;;
;;
;; String insertions

(defn line-count [s]
  (loop [lines 0
         index 0]
    (let [next-i (.indexOf s \newline index)]
      (if (= -1 next-i)
        lines
        (recur (inc lines)
               (inc next-i))))))

(defn- insert-at-index [s i to-insert]
  (str (subs s 0 i)
       to-insert
       (subs s i)))

(defn- line-index
  ([s line] (line-index s line 0))
  ([^string s line start]
   (assert (not (neg? line)))
   (loop [line line
          last-i start]
     (if (zero? line)
       last-i
       (let [next-i (.indexOf s \newline last-i)]
         (if (= -1 next-i)
           (.-length s)
           (recur (dec line)
                  (inc next-i))))))))

(defn- some-index-of [s index start]
  (let [i (.indexOf s index start)]
    (when-not (= -1 i)
      i)))

(defn- col-index [s column start]
  (let [next-line (or (some-index-of s \newline start)
                      (.-length s))]
    (max start
         (min (+ column start)
              next-line))))

(defn- pos-index [s line column]
  (let [line-i (line-index s line)
        col-i (col-index s column line-i)]
    col-i))

(defn insert-at-coords [s [line column] to-insert]
  (insert-at-index s (pos-index s line column) to-insert))

(defn string-offset [s]
  (let [last-i (.lastIndexOf s \newline)]

    (if (= -1 last-i)
      [0 (.-length s)]
      [(line-count s)
       (- (.-length s) (inc last-i))])))

(defn add-offsets [o & offsets]
  (reduce (fn [[a1 b1] [a2 b2]]
            [(+ a1 a2) (+ b1 b2)])
          o offsets))

(comment

 (= (string-offset "a\n") [1 0])
 (= (string-offset "\n") [1 0])
 (= (string-offset "a") [0 1])
 (= (string-offset "\na") [1 1])

 ;; line index
 (assert (= (mapv (partial line-index "ABC\nD\nE")
                  [0 1 2 3 4])
            [0 4 6 7 7]))

 ;; col index
 (assert (= (mapv #(apply col-index "AB\nCD" %)
                  [[0 0]
                   [1 0]
                   [10 0]
                   [0 3]
                   [1 3]])
            [0 1 2 3 4]))

 (assert (= (insert-at-pos "a\n" [0 2] \x)
            "ax\n"))

 (assert (= (insert-at-pos "a\n" [1 0] \x)
            (insert-at-pos "a\n" [1 2] \x)
            (insert-at-pos "a\n" [2 0] \x)
            "a\nx"))

 (assert (= (insert-at-pos "a" [0 0] \x)
            "xa"))

 (assert (= (insert-at-pos "a" [0 1] \x)
            (insert-at-pos "a" [0 999] \x)
            "ax"))
 (assert (= (insert-at-pos "a\n" [0 1] \x)
            (insert-at-pos "a\n" [0 999] \x)
            "ax\n"))

 (assert (= (insert-at-pos "a\nb" [1 0] \x)
            "a\nxb"))

 (assert (= (insert-at-pos "a\nb" [1 1] \x)
            (insert-at-pos "a\nb" [1 999] \x)
            "a\nbx")))

