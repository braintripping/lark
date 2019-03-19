(ns lark.tree.nav
  (:refer-clojure :exclude [range])
  (:require [fast-zip.core :as z]
            [lark.tree.node :as n]
            [lark.tree.reader :as rd]
            [lark.tree.range :as range]
            [chia.util.perf :as perf]
            [chia.util :as u]))

#_(defn prefix-is-integral [tag]
    (perf/keyword-in? [:set
                       :fn
                       :regex] tag))

(defn has-prefix? [tag]
  (some-> (rd/edges tag)
          (count)
          (= 1))
  #_(some-> (rd/edges tag)
            (count)
            (pos?)))

(defn include-prefix-parents [loc]
  (if (and loc
           (nil? (z/left loc))
           (some-> (z/up loc)
                   (.-node)
                   (.-tag)
                   (has-prefix?)))
    (include-prefix-parents (z/up loc))
    loc))

(defn iteratev-while [f start-loc]
  (when start-loc
    (loop [loc start-loc
           out (transient [start-loc])]
      (if-let [next-loc (f loc)]
        (recur next-loc (conj! out next-loc))
        (persistent! out)))))

(defn iteratev-nav [nav f start-loc]
  (when start-loc
    (loop [loc start-loc
           out (transient [start-loc])]
      (if-let [next-loc (nav loc)]
        (recur next-loc (conj! out (f next-loc)))
        (persistent! out)))))

(defn child-locs [loc]
  (iteratev-while z/right (z/down loc)))

(defn right-locs [loc]
  (iteratev-while z/right (include-prefix-parents (z/right loc))))

(defn left-locs [loc]
  (iteratev-while z/left (include-prefix-parents (z/left loc))))

(defn right-up [loc]
  (or (z/right loc)
      (some-> (z/up loc)
              (include-prefix-parents))))

(defn left-up [loc]
  (or (z/left loc)
      (some-> (z/up loc)
              (include-prefix-parents))))

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
                     (some-> (filter #(range/within? % pos) (child-locs loc))
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

(defn mouse-eval-region
  "Select sexp under the mouse. Whitespace defers to parent."
  [loc]
  (or (and (n/sexp? (.-node loc)) loc)
      (z/up loc)))

(defn top-loc [loc]
  (loop [loc loc]
    (if-not loc
      loc
      (if (or (= :base (:tag (.-node loc)))
              (= :base (some-> (z/up loc) .-node :tag)))
        loc
        (recur (z/up loc))))))

(defn closest [pred loc]
  (if-not loc
    nil
    (if (pred loc)
      loc
      (recur pred (z/up loc)))))

(defn cursor-space-loc [zipper pos]
  (when-let [loc (navigate zipper pos)]
    (->> [loc (z/left loc)]
         (keep identity)
         (filter #(-> %
                      (.-node)
                      (.-tag)
                      #{:space
                        :newline}))
         (first))))

(defn path-node-pred [node]
  (or (= (.-tag node) :newline)
      (not (rd/whitespace-tag? (.-tag node)))))

(defn get-loc [zipper path]
  (loop [segments path
         loc zipper]
    (if (or (not loc)
            (empty? segments))
      loc
      (recur (rest segments)
             (->> (iterate z/right (z/down loc))
                  (take-while identity)
                  (filter (comp path-node-pred z/node))
                  (drop (first segments))
                  (first))))))

(defn get-path [loc]
  (loop [loc loc
         out ()]
    (if-not loc
      (drop 1 out)
      (recur (z/up loc)
             (cons (->> (z/lefts loc)
                        (filter path-node-pred)
                        (count)) out)))))

(defn find-next [loc pred]
  (->> (iterate z/next loc)
       (take-while #(and % (not (z/end? %))))
       (filter (comp pred z/node))
       (first)))

(defn left-node [^z/ZipperLocation loc]
  (-> (.-path loc) .-l peek))

(defn right-node [^z/ZipperLocation loc]
  (some-> (.-path loc)
          .-r
          first))

(defn up-node
  "Returns the loc of the parent of the node at this loc, or nil if at the top"
  [^z/ZipperLocation loc]
  (let [^z/ZipperPath path (.-path loc)]
    (some-> (and path (.-pnodes path))
            (peek))))

(defn prev-whitespace-loc [pos loc]
  (->> [loc (z/left loc)]
       (sequence (comp (keep identity)
                       (filter (comp n/whitespace? z/node))
                       (remove #(range/at-start? pos (z/node %)))))
       (first)))

(defn prev-whitespace-node [pos loc]
  (some-> (prev-whitespace-loc pos loc)
          (z/node)))