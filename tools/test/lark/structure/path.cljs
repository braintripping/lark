(ns lark.structure.path
  (:refer-clojure :exclude [contains? resolve])
  (:require [clojure.spec.alpha :as s]))

(defn update-last [path f]
  (update path (dec (count path)) f))

(defn update-nth [path n f]
  (update path n f))

(defn append [path x]
  (conj path x))

(defn drop-common [p1 p2]
  (let [c1 (count p1)
        c2 (count p2)]
    (loop [i 0]
      (if (and (> c1 i)
               (> c2 i)
               (= (nth p1 i)
                  (nth p2 i)))
        (recur (inc i))
        [(subvec p1 i)
         (subvec p2 i)]))))

(defn compare-paths
  [x y]
  (or (first (drop-while zero? (mapv compare x y)))
      (- (count x)
         (count y))))

(defn before? [p1 p2]
  (neg? (compare-paths p1 p2)))

(defn after? [p1 p2]
  (pos? (compare-paths p1 p2)))

(defn ancestor? [parent child]
  (let [pcount (count parent)]
    (and (< pcount (count child))
         (= parent
            (subvec child 0 pcount)))))

(defn within? [child parent]
  (let [pcount (count parent)]
    (and (<= pcount (count child))
         (= parent
            (subvec child 0 pcount)))))

(defn move-path [from-p to-p child-offset path]
  (if (within? path from-p)
    (into to-p
          (when-let [inner-path (seq (drop (count from-p) path))]
            (into [(+ (first inner-path) child-offset)]
                  (rest inner-path))))
    path))

(defn starts-with? [path prefix]
  (let [pc (count prefix)]
    (and (>= (count path) pc)
         (= prefix (subvec path 0 pc)))))

(defn parent [path]
  (let [pc (count path)]
    (if (zero? pc)
      nil
      (subvec path 0 (dec pc)))))

(defn sibling? [p1 p2]
  (and (= (count p1)
          (count p2))
       (= (parent p1)
          (parent p2))))


(defn depths [path]
  (map #(subvec path 0 %) (range 1 (inc (count path)))))

(s/def ::segment (s/and int?
                        (complement neg?)))

(s/def ::path (s/and vector?
                     (s/coll-of ::segment)))


(s/fdef before?
        :args (s/coll-of ::path)
        :ret boolean?)

(s/fdef ancestor?
        :args (s/coll-of ::path)
        :ret boolean?)

(s/fdef drop-common
        :args (s/coll-of ::path))

(s/fdef update-last
        :args (s/cat :path ::path
                     :fn fn?))
(s/fdef update-nth
        :args (s/cat :path ::path
                     :index number?
                     :fn fn?))
(s/fdef append
        :args (s/cat :path ::path
                     :segment number?))

(s/fdef depths
        :args (s/cat :path ::path)
        :ret (s/coll-of ::path))