(ns lark.cards.structure.path
  (:refer-clojure :exclude [contains?])
  (:require [clojure.spec.alpha :as s]))

(defn drop-common [c1 c2]
  (loop [c1 (seq c1)
         c2 (seq c2)]
    (if (and c1
             c2
             (= (first c1)
                (first c2)))
      (recur (next c1)
             (next c2))
      [c1 c2])))

(s/def ::path (s/coll-of number?))

(defn compare-path [p1 p2 comparator]
  (loop [p1 (seq p1)
         p2 (seq p2)]
    (cond (= p1 p2) false
          (empty? p1) true
          (comparator (first p1)
                      (first p2)) true
          :else
          (recur (rest p1)
                 (rest p2)))))

(defn before? [p1 p2]
  (compare-path p1 p2 <))

(s/fdef before?
        :args (s/coll-of ::path)
        :ret boolean?)

(defn contains? [parent child]
  (and
   (not= parent child)
   (= parent (take (count parent) child))))

(s/fdef contains?
        :args (s/coll-of ::path)
        :ret boolean?)

(defn parent [path]
  (assert (seq path))
  (drop-last path))

(defn after? [p1 p2]
  (or (contains? p2 p1)
      (before? p2 p1)))