(ns lark.cards.structure.delta)

(defmacro track-deltas
  "Transact operations on an editor state, tracking shifts and offsets.

   Operations must be performed in order (left to right).

   Use with delta/add! & delta/select!."
  [& body]
  `(binding [~'lark.cards.structure.delta/*deltas* (volatile! {:accrued-shifts {}
                                                               :accrued-offsets {}})]
     (let [state# (do ~@body)
           {shifts# :accrued-shifts
            offsets# :accrued-offsets} @~'lark.cards.structure.delta/*deltas*]
       (-> state#
           (cond-> (or (seq shifts#)
                       (seq offsets#))
                   (update :ast ~'lark.tree.emit/materialize))))))

(defmacro track-selections
  "Transact operations on an editor state, tracking shifts and offsets.

   Operations must be performed in order (left to right).

   Use with delta/add! & delta/select!."
  [& body]
  `(binding [~'lark.cards.structure.delta/*selections* (volatile! [])]
     (let [state# (do ~@body)
           selections# @~'lark.cards.structure.delta/*selections*]
       (-> state#
           (assoc :selections selections#)))))

(defmacro track
  "Transact operations on an editor state, tracking shifts and offsets.

   Operations must be performed in order (left to right).

   Use with delta/add! & delta/select!."
  [& body]
  `(let [prev-deltas# ~'lark.cards.structure.delta/*deltas*]
     (binding [~'lark.cards.structure.delta/*deltas* (volatile! (if prev-deltas#
                                                                  @prev-deltas#
                                                                  {:accrued-shifts {}
                                                                   :accrued-offsets {}}))]
       (let [value# (do ~@body)
             deltas# @~'lark.cards.structure.delta/*deltas*]
         (when prev-deltas#
           (vreset! prev-deltas# deltas#))
         [value# deltas#]))))