(ns lark.cards.structure.delta
  (:refer-clojure :exclude [apply resolve])
  (:require [chia.util :as u]
            [lark.tree.emit :as emit]                       ;; for macro
            [lark.cards.structure.path :as path]
            [lark.cards.structure.coords :as coords]
            [cljs.spec.alpha :as s])
  (:require-macros [lark.cards.structure.delta]))

(def ^:dynamic *deltas* nil)
(def ^:dynamic *selections* nil)

(defn path-depths [path]
  (map #(take % path) (range 1 (inc (count path)))))

(s/fdef path-depths
        :args (s/cat :path ::path/path)
        :ret (s/coll-of ::path/path))

(s/def ::shift (s/tuple number? number?))                   ;; index, delta

(defn- apply-shifts
  "Apply `shifts` to `path`"
  [shifts path]
  (for [path (path-depths path)
        :let [shifts (get shifts (drop-last path))]]
    (->> shifts (reduce
                 (fn [i [-i delta]]
                   (cond-> i
                           (<= -i i) (+ delta))) (last path)))))

(s/fdef apply-shifts
        :args (s/cat :shifts (s/map-of ::path/path (s/coll-of ::shift))
                     :path ::path/path))

(comment
 (apply-shifts '(1 2 3) {'() [[0 1]]                        ;; add 1 to children of root
                         '(1) [[0 1]                        ;; do nothing to children of (1)
                               [1 -1]]
                         '(1 2) [[3 -1]]}))

(defn add
  "Adds new shifts and deltas, apply shifts to existing paths"
  [{:as deltas
    :keys [accrued-shifts
           accrued-offsets]} {:keys [shifts
                                     offsets]}]
  {:accrued-shifts (-> accrued-shifts
                       #_(u/update-keys (partial apply-shifts shifts)) ;; shift paths
                       (as-> m (merge-with into m shifts))) ;; merge new shifts
   :accrued-offsets (-> accrued-offsets
                        #_(u/update-keys (partial apply-shifts shifts)) ;; shift paths
                        (as-> m (merge-with coords/+ m offsets)))})

(defn add! [new-deltas]
  ;(prn :add! new-deltas)
  (assert *deltas* "delta/add! must be called from within a transaction")
  (vswap! *deltas* add new-deltas))


(defn shift [path added]
  {path [[(or (last path) 0) added]]})

(s/fdef shift
        :ret (s/map-of ::path/path
                       (s/coll-of ::shift)))

(defn shift! [path added]
  (add! {:shifts (shift path added)}))

(s/fdef shift!
        :args (s/cat :path ::path/path
                     :added number?))

(defn set-offset! [path offset]
  (vswap! *deltas* assoc-in [:accrued-offsets path] offset))

(defn offset! [path offset]
  (add! {:offsets {path offset}}))

(defn get-offset [path]
  (get-in @*deltas* [:accrued-offsets path]))

(defn selections! [selections]
  (vswap! *selections* into selections))

(defn select-start! [selection]
  (assert (and (:from selection) (not (:to selection))))
  (vswap! *selections* conj selection))

(defn select-end! [selection]
  (assert (and (:to selection) (not (:from selection))))
  (vswap! *selections* (fn [sels]
                         (update sels (dec (count sels)) (fn [sel]
                                                           (assert (and (:from sel) (not (:to sel))))
                                                           (merge sel selection))))))

(defn cursor! [selection]
  (assert (and (:from selection) (not (:to selection))))
  (vswap! *selections* conj selection))
