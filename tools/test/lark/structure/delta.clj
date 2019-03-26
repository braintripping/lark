(ns lark.structure.delta)

(def deltas 'lark.structure.delta/*deltas*)

(defmacro track
  "Transact operations on an editor state, tracking shifts and offsets.

   Operations must be performed in order (left to right).

   Use with delta/add! & delta/select!."
  [selections & body]
  `(binding [~deltas (volatile! {:selections (mapv ~'lark.structure.delta/tracked* ~selections)
                                 :tracked []})]

     (let [state# (do ~@body)
           {selections# :selections} @~deltas]
       (-> state#
           (assoc :selections (mapv deref selections#))
           (update :ast ~'lark.tree.emit/materialize)))))

