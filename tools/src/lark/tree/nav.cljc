(ns lark.tree.nav
  (:refer-clojure :exclude [range])
  (:require [fast-zip.core :as z]
            [lark.tree.node :as n]
            [lark.tree.reader :as rd]
            [lark.tree.range :as range]))

(def prefix-parents (reduce-kv (fn [parents tag [_ right-bracket]]
                                 (if (nil? right-bracket)
                                   (conj parents tag)
                                   parents)) #{} rd/edges))

(defn include-prefix-parents [loc]
  (when loc
    (if (contains? prefix-parents (some-> (z/up loc)
                                          (z/node)
                                          (get :tag)))
      (include-prefix-parents (z/up loc))
      loc)))

(defn iterate-including [f x]
  (iterate f (f x)))

(defn child-locs [loc]
  (take-while identity (iterate z/right (z/down loc))))

(defn right-up [loc]
  (or (z/right loc)
      (some-> (z/up loc)
              (include-prefix-parents))))

(defn left-up [loc]
  (or (z/left loc)
      (some-> (z/up loc)
              (include-prefix-parents))))

(defn right-locs [loc]
  (->> (z/right loc)
       (include-prefix-parents)
       (iterate z/right)
       (take-while identity)))

(defn left-locs [loc]
  (->> (z/left loc)
       (include-prefix-parents)
       (iterate z/left)
       (take-while identity)))

(defn before? [pos1 pos2]
  (and (<= (get pos1 :line) (get pos2 :line))
       (< (get pos1 :column) (get pos2 :column))))

(defn after? [pos1 pos2]
  (and (>= (get pos1 :line) (get pos2 :line))
       (> (get pos1 :column) (get pos2 :column))))

(defn navigate
  "Navigate to a position within a zipper (returns loc) or ast (returns node)."
  [ast pos]
  (assert pos)
  (if (= (type ast) z/ZipperLocation)
    (let [loc ast
          {:keys [children] :as node} (z/node loc)
          found (when (range/within? node pos)
                  (if
                   (or (n/terminal-node? node)
                       (empty? children))
                    loc
                    (or
                     (some-> (filter #(range/within? % pos) (child-locs loc))
                             first
                             (navigate pos))
                     loc)))]
      (if (let [found-node (some-> found z/node)]
            (and (= (get pos :line) (get found-node :end-line))
                 (= (get pos :column) (get found-node :end-column))))
        (or (z/right found) found)
        found))
    (when (range/within? ast pos)
      (if
       (or (n/terminal-node? ast)
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
  (or (and (n/sexp? (z/node loc)) loc)
      (z/up loc)))

(defn top-loc [loc]
  (loop [loc loc]
    (if-not loc
      loc
      (if (or (= :base (:tag (z/node loc)))
              (= :base (some-> (z/up loc) z/node :tag)))
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
                      (z/node)
                      (get :tag)
                      (= :space)))
         (first))))

(defn path-node-pred [{:keys [tag]}]
  (or (= tag :newline)
      (not (rd/whitespace-tag? tag))))

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

(defn find-next [ast pred]
  (->> (iterate z/next ast)
       (take-while #(and % (not (z/end? %))))
       (filter (comp pred z/node))
       (first)))