(ns lark.cards.structure.coords
  (:refer-clojure :exclude [- +])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [spell-spec.alpha :as ss]))

(s/def ::coord (s/spec
                (s/cat :line (s/nilable number?)
                       :col (s/nilable number?)
                       :end? (s/? (s/nilable boolean?)))))

(s/def ::from (s/nilable ::coord))
(s/def ::to (s/nilable ::coord))

(s/def ::span
  (ss/strict-keys
   :opt-un [::from
            ::to]))

(defn - [o & offsets]
  (reduce (fn [[a1 b1] [a2 b2]]
            [(core/- a1 a2) (core/- b1 b2)])
          o offsets))

(s/fdef -
        :args (s/+ ::coord)
        :ret ::coord)

(defn + [o & offsets]
  (reduce (fn [[a1 b1] [a2 b2]]
            [(core/+ a1 a2) (core/+ b1 b2)])
          o offsets))

(s/fdef +
        :args (s/+ ::coord)
        :ret ::coord)

