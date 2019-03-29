(ns lark.structure.path
  "Operations for efficiently comparing and modifying paths"
  (:refer-clojure :exclude [contains? resolve compare = < > >= <= last])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [goog.array :as garray]))

;;;;;;;;;;;;;;;;;
;;
;; Reading

(defn last
  "Efficiently read last segment of path"
  [path]
  (let [pcount (count path)]
    (if (zero? pcount)
      nil
      (nth path (dec pcount)))))

(defn parent
  "Returns parent of path"
  [path]
  (let [pc (count path)]
    (if (zero? pc)
      nil
      (subvec path 0 (dec pc)))))

(defn drop-common
  "Returns path1 and path2 with common ancestors removed"
  [path1 path2]
  (let [c1 (count path1)
        c2 (count path2)]
    (loop [i 0]
      (if (and (core/> c1 i)
               (core/> c2 i)
               (identical? (nth path1 i)
                           (nth path2 i)))
        (recur (inc i))
        [(subvec path1 i)
         (subvec path2 i)]))))

;;;;;;;;;;;;;;;;;
;;
;; Left-to-right comparison

(defn- ^number compare-segment
  "Comparator for segments, handles :end values."
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

    (keyword-identical? x :end) (if (keyword-identical? y :end) 0 1)
    :else
    (throw (js/Error. (str "Cannot compare " x " to " y)))))

(defn- ^number compare
  "Comparator for paths"
  [p1 p2]
  (if (identical? p1 p2)
    0
    (let [iter1 (-iterator p1)
          iter2 (-iterator p2)]
      (loop []
        (let [has-x ^boolean (.hasNext iter1)
              has-y ^boolean (.hasNext iter2)]
          (if
           (or has-x has-y)
            (let [cmp (compare-segment (when has-x (.next iter1))
                                       (when has-y (.next iter2)))]
              (if (zero? cmp)
                (recur)
                cmp))
            0))))))

(defn ^boolean < [p1 p2]
  (neg? (compare p1 p2)))

(defn ^boolean <= [p1 p2]
  (not (pos? (compare p1 p2))))

(defn ^boolean > [p1 p2]
  (pos? (compare p1 p2)))

(defn ^boolean >= [p1 p2]
  (not (neg? (compare p1 p2))))

(defn = [p1 p2]
  (cond (identical? p1 p2) true
        (not (identical? (count p1) (count p2))) false
        :else (zero? (compare p1 p2))))

(defn sibling? [p1 p2]
  (and (core/= (count p1)
               (count p2))
       (= (parent p1)
          (parent p2))))

;;;;;;;;;;;;;;;;;
;;
;; Depth comparison

(defn ^boolean ancestor? [parent child]
  (let [pcount (count parent)]
    (and (core/> (count child) pcount)
         (= parent
            (subvec child 0 pcount)))))

(defn ^boolean starts-with?
  "Returns true if first segments of `path` are equal to `prefix`"
  [child parent]
  (let [pcount (count parent)]
    (and (core/>= (count child) pcount)
         (= parent (subvec child 0 pcount)))))

;;;;;;;;;;;;;;;;;
;;
;; Updating

(defn- update-segment
  "Update segment, ignoring :end segments"
  [segment f & args]
  (if (keyword-identical? segment :end)
    segment
    (apply f segment args)))

(defn update-nth [path n f]
  (update path n update-segment f))

(defn update-last [path f]
  (update-nth path (dec (count path)) f))

(defn append [path x]
  (assert (not (keyword-identical? :end (core/last path)))
          "Cannot append to an :end path")
  (conj path x))

(defn move-path [from-p to-p child-offset path]
  (if (starts-with? path from-p)
    (into to-p
          (when-let [inner-path (seq (drop (count from-p) path))]
            (into [(+ (first inner-path) child-offset)]
                  (rest inner-path))))
    path))



(s/def ::segment (s/or :number (s/and int?
                                      (complement neg?))
                       :end #{:end}))

(s/def ::path (s/and vector?
                     (s/coll-of ::segment)))


(s/fdef <
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


(comment

 (let [paths1 [[0 1 0 3]
               [0 0 1 4]
               [0 0 4]
               [0 1 0 :end]
               [1]
               [1 0 10 3]
               [1 1]
               [1 1 1]
               [1 :end]
               [:end]
               []
               []
               []]
       paths2 (shuffle paths1)]

   (simple-benchmark [f compare]
                     (doseq [p1 paths1
                             p2 (shuffle paths1)]
                       (f p1 p2))
                     10000)))