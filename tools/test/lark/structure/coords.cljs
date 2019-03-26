(ns lark.structure.coords
  (:refer-clojure :exclude [- + > < >= <= =])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [spell-spec.alpha :as ss]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]))

(s/def ::coord (s/and vector?
                      (s/coll-of number?))
  #_(s/spec
                (s/cat :line (s/nilable number?)
                       :col (s/nilable number?))))

(s/def ::from (s/nilable ::coord))
(s/def ::to (s/nilable ::coord))

(s/def ::span
  (ss/strict-keys
   :opt-un [::from
            ::to]))

(defn - [o & coords]
  (reduce (fn [[a1 b1] [a2 b2]]
            [(core/- a1 a2) (core/- b1 b2)])
          o coords))

(defn + [o & coords]
  (reduce (fn [[a1 b1] [a2 b2]]
            [(core/+ a1 a2) (core/+ b1 b2)])
          o coords))

(defn > [[l1 c1] [l2 c2]]
  (or (core/> l1 l2)
      (and (core/= l1 l2)
           (core/> c1 c2))))

(defn >= [[l1 c1] [l2 c2]]
  (or (core/> l1 l2)
      (and (core/= l1 l2)
           (core/>= c1 c2))))

(defn < [[l1 c1] [l2 c2]]
  (or (core/< l1 l2)
      (and (core/= l1 l2)
           (core/< c1 c2))))

(defn <= [[l1 c1] [l2 c2]]
  (or (core/< l1 l2)
      (and (core/= l1 l2)
           (core/<= c1 c2))))

(defn = [[l1 c1] [l2 c2]]
  (and (identical? l1 l2)
       (identical? c1 c2)))

(defn point? [{:keys [from to]}]
  (or (nil? to)
      (= from to)))

(defn inner-span [{:as node
                   [line col end-line end-col] :range}]
  (if-let [[L R] (rd/edges (.-tag node))]
    {:from [line
            (core/+ col (count L))]
     :to [end-line
          (core/- end-col (count R))]}
    node))

(defn within-right-edge? [node coords]
  (when-let [right-edge (second (rd/edges (.-tag node)))]
    (let [right-outer (:to node)
          right-inner (- right-outer [0 (count right-edge)])]
      (and (>= coords right-inner)
           (<= coords right-outer)))))

(defn offset [[node-line node-col] [pos-line pos-col]]
  [(core/- pos-line node-line)
   (if (core/= node-line pos-line)
     (core/- pos-col node-col)
     pos-col)])

(defn clamp
  "If `coords` is outside bounds of `node`, constrains to fit."
  [[pline pcol :as coords] {:as node
                            :keys [tag
                                   from
                                   to]}]
  (let [[line col] from]
    (case tag
      ;; pos is outside of base node
      :base (cond (< coords from) from
                  (> coords to) to
                  :else coords)
      ;; pos has been adjusted within a newline node, needs to move to correct line
      :newline (cond (neg? pcol) from
                     (and (core/= pline line)
                          (core/> pcol col)) [(inc line)
                                              (dec (core/- pcol col))]
                     :else coords)
      coords)))

(s/fdef -
        :args (s/+ ::coord)
        :ret ::coord)

(s/fdef +
        :args (s/+ ::coord)
        :ret ::coord)


