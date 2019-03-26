(ns lark.structure.path
  (:refer-clojure :exclude [contains? resolve compare])
  (:require [clojure.spec.alpha :as s]
            [goog.array :as garray]))

(defn equal [p1 p2]
  (cond (identical? p1 p2) true
        (not (== (count p1) (count p2))) false
        :else
        (let [me-iter (-iterator p1)
              you-iter (-iterator p2)]
          (loop []
            (if ^boolean (.hasNext me-iter)
              (let [x (.next me-iter)
                    y (.next you-iter)]
                (if (or (identical? x y)
                        (and (keyword? x) (keyword? y) (keyword-identical? x y)))
                  (recur)
                  false))
              true)))))

(defn get-last [path]
  (let [pcount (count path)]
    (if (zero? pcount)
      nil
      (nth path (dec pcount)))))

(defn- update-segment [segment f & args]
  (if (keyword-identical? segment :end)
    segment
    (apply f segment args)))

(defn update-nth [path n f]
  (update path n update-segment f))

(defn update-last [path f]
  (update-nth path (dec (count path)) f))

(defn append [path x]
  (assert (not (keyword-identical? :end (last path)))
          "Cannot append to an :end path")
  (conj path x))

(defn drop-common [p1 p2]
  (let [c1 (count p1)
        c2 (count p2)]
    (loop [i 0]
      (if (and (> c1 i)
               (> c2 i)
               (identical? (nth p1 i)
                           (nth p2 i)))
        (recur (inc i))
        [(subvec p1 i)
         (subvec p2 i)]))))

(defn ^number compare-segment
  "Comparator, handles :end values."
  [x y]
  (cond
    (identical? x y) 0

    (nil? x) -1

    (nil? y) 1


    (number? x) (if (number? y)
                  (garray/defaultCompare x y)
                  (if (keyword-identical? y :end)
                    -1
                    (throw (js/Error. (str "Cannot compare " x " to " y)))))

    (keyword-identical? x :end) (if (keyword-identical? y :end)
                                  0
                                  1)
    :else
    (throw (js/Error. (str "Cannot compare " x " to " y)))))

(defn compare
  [x y]
  (or (first (drop-while zero? (mapv compare-segment x y)))
      (- (count x)
         (count y))))

(comment
 (let [paths [[0]
              [0 0]
              [0 1]
              [0 :end]
              [1]
              [1 0]
              [1 1]
              [1 1 1]
              [1 :end]
              [:end]]]

   (assert
    (= paths
       (sort-by identity compare (shuffle paths))))))

(defn before? [p1 p2]
  (neg? (compare p1 p2)))

(defn after? [p1 p2]
  (pos? (compare p1 p2)))

(defn ancestor? [parent child]
  (let [pcount (count parent)]
    (and (< pcount (count child))
         (equal parent
                (subvec child 0 pcount)))))

(defn within? [child parent]
  (let [pcount (count parent)]
    (and (<= pcount (count child))
         (equal parent
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
         (equal prefix (subvec path 0 pc)))))

(defn parent [path]
  (let [pc (count path)]
    (if (zero? pc)
      nil
      (subvec path 0 (dec pc)))))

(defn sibling? [p1 p2]
  (and (= (count p1)
          (count p2))
       (equal (parent p1)
              (parent p2))))


(defn depths [path]
  (map #(subvec path 0 %) (range 1 (inc (count path)))))

(s/def ::segment (s/or :number (s/and int?
                                      (complement neg?))
                       :end #{:end}))

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