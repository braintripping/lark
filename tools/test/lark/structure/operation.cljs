(ns lark.structure.operation
  (:require [lark.structure.operation.impl :as impl]
            [lark.structure.delta :as delta]
            [lark.tree.reader :as rd]
            [cljs.spec.alpha :as s]

   ;; for side effects (defmethods)
            lark.structure.operation.edit
            lark.structure.operation.select))

(defn operate [op state & args]
  (let [state (-> (delta/track (:selections state)
                               (apply impl/-operate op state args))
                  (update :selections distinct))]
    (assert (instance? rd/Node (:ast state))
            (str "did not return an AST: " op args (:ast state)))
    state))

(defmethod impl/-operate :identity
  [_ state]
  state)

#_(defmethod impl/operate :cursor/hop
    [_ {:as state
        :keys [ast
               selections]} direction]
    (let [loc (tree/zip ast)]
      (assoc state :selections
                   (->> selections
                        (reduce (fn [out [from to :as selection]]
                                  (conj out
                                        (case axis
                                          :x (if to
                                               (if (pos? distance)
                                                 [to]
                                                 [from])
                                               [(-> (path->pos loc from)
                                                    (update :column + distance)
                                                    (->> (pos->path loc)))])
                                          :y state))) [])
                        (distinct)))))

(s/def ::ast #(instance? rd/Node %))
(s/def ::state (s/keys :req-un [::ast]))
(s/fdef operate
        :ret ::state)