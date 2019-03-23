(ns lark.cards.structure.pointer
  (:refer-clojure :exclude [range resolve])
  (:require [fast-zip.core :as z]
            [lark.cards.structure.coords :as coords]
            [lark.cards.structure.loc :as loc]
            [lark.cards.structure.path :as path]
            [lark.cards.structure.delta :as delta]
            [lark.cards.structure.position :as pos]
            [spell-spec.alpha :as ss]
            [cljs.spec.alpha :as s]
            [lark.tree.range :as range]
            [chia.util :as u]))

(s/def ::pointer (s/tuple ::path/path
                          ::coords/coord))

(s/def ::from ::pointer)
(s/def ::to ::pointer)

(s/def ::span
  (ss/strict-keys
   :opt-un [::from
            ::to]))

(defn position [root-loc [path offset]]
  (let [loc (loc/get-loc root-loc path)
        node (some-> loc z/node)]
    (pos/resolve-offset offset node)))

(s/fdef position
        :args (s/cat :loc ::loc/loc
                     :pointer ::pointer))

(defn range [loc {:keys [from to]}]
  (assert (when loc
            (instance? z/ZipperLocation loc)))
  (merge
   (range/bounds (position loc from) :left)
   (when to
     (range/->end (position loc to)))))

(s/fdef range
        :args (s/cat :loc ::loc/loc
                     :sel ::span))

(defn resolve
  [[path offset]]
  (let [deltas @delta/*deltas*
        path (if-let [accrued (:accrued-shifts deltas)]
               (delta/apply-shifts accrued path)
               path)
        offset (if-let [accrued (get (:accrued-offsets deltas) path)]
                 (coords/+ accrued offset)
                 offset)]
    [path offset]))

(s/fdef resolve
        :args (s/cat :pointer ::pointer))

(defn resolve-span
  [span]
  (u/update-some-keys span [:from
                            :to] resolve))

(s/fdef resolve-span
        :args (s/cat :selection ::span))