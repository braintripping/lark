(ns lark.structure.operation
  (:require [lark.structure.delta :as delta]
            [lark.tree.reader :as rd]
            [cljs.spec.alpha :as s]

            [lark.structure.operation.edit :as edit]
            [lark.structure.operation.select :as select])
  (:require-macros [lark.structure.operation :as o]))

(defonce registry (atom {}))

(defn operate [op state & args]
  (let [operation (@registry op)
        state (apply operation state args)]
    (assert (instance? rd/Node (:ast state))
            (str "did not return an AST: " op args (:ast state)))
    state))

(o/defop :identity identity)
(o/defop :edit/replace edit/replace)
(o/defop :selection/set-by-coords select/set-by-coords)
(o/defop :selection/move select/move)

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
        :args (s/cat :state ::state)
        :ret ::state)