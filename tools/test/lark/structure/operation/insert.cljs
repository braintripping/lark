(ns lark.structure.operation.insert
  (:require [lark.structure.path :as path]
            [fast-zip.core :as z]
            [lark.structure.loc :as loc]
            [lark.tree.core :as tree]
            [lark.structure.delta :as delta]
            [lark.structure.operation.impl :as impl]
            [lark.tree.emit :as emit]
            [cljs.pprint :as pp]
            [lark.structure.string :as string]
            [lark.tree.parse :as parse]
            [chia.util :as u]
            [lark.structure.coords :as coords]
            [lark.structure.pointer :as pointer]
            [cljs.spec.alpha :as s]
            [lark.tree.node :as n]))


(defn replace-in-terminal! [loc from-coords to-coords text]
  (if-not (n/terminal? (z/node loc))
    (do
      #_(prn :passing-not-terminal (emit/string loc))
      loc)
    (do
      (delta/print-selections)
      (let [text-before (emit/string (z/node loc))
            from-coords (or from-coords [0 0])
            to-coords (or to-coords (string/end-coords text-before))
            path (loc/path loc)
            insertion-point (delta/tracked {:from [path to-coords]})
            text-after (string/replace-at-coords text-before
                                                 {:from from-coords
                                                  :to to-coords}
                                                 text)
            nodes-after (:children (parse/ast text-after))
            loc (if (u/some-str text-after)
                  (loc/replace-splice! loc nodes-after)
                  (loc/remove-loc! loc))
            offset (-> from-coords
                       (coords/- to-coords)
                       (coords/+ (string/end-coords text)))]
        #_(prn :replace-in-terminal offset :from (:from @insertion-point))

        ;; no, we shouldn't add an offset to the _parent_ that we _inserted_ this into...
        (delta/add-offset! (:from @insertion-point) offset)
        (delta/print-selections)
        loc))))

(defn merge-between! [loc span]
  (print "\n\n\n\n" :merge-between (emit/string loc) @span)
  (let [{[from _] :from
         [to _] :to} @span]
    (assert (path/before? from to)))
  (loop [ploc nil
         loc (loc/nav loc (first (:from @span)))
         i 0]
    (let [to (first (:to @span))]
      (if (> i 20)
        (throw (js/Error. "Looping"))
        (let [node (z/node loc)
              p (loc/path loc)]

          (print (:tag node) (emit/string node) :path p :$to to :join? (when (and ploc loc (loc/joinable? ploc loc)) (map emit/string [ploc loc])) "       " :to to)

          (cond (and (= p to)
                     (nil? ploc)) loc

                (and ploc
                     loc
                     (loc/joinable? ploc loc))
                (recur nil (loc/join! ploc loc) (inc i))

                ;;; need to fix path/after?, likely is broken
                (path/after? p to) (do
                                     (prn :too-far p :> to)
                                     (throw (js/Error. "Navigated too far")))

                :else
                (recur loc (loc/next-between loc to) (inc i))))))))
(comment
 (-> (delta/track []
                  (let [loc (tree/string-zip "[a][b]")
                        joined (loc/join! (z/down loc)
                                          (-> loc z/down z/right))]
                    {:ast (z/root joined)}))
     :ast
     emit/string))

(defn remove-between! [loc span]
  (loop [loc (loc/nav loc (first (:from @span)))
         i 0]
    (assert loc "remove-between loc")
    (let [p (loc/path loc)
          {[from _] :from
           [to _] :to} @span]
      (when (> i 100) (throw (js/Error. "Looping")))

      (cond (= p to) loc
            (= p from) (recur (loc/next-between loc to) (inc i))
            (or (path/ancestor? p from)
                (path/ancestor? p to)) (if-let [nloc (loc/next-between loc to)]
                                         (recur nloc (inc i))
                                         (throw (js/Error. (str "not-found-next"
                                                                )
                                                           )))
            :else
            (recur (loc/remove-loc! loc) (inc i))))))

(comment
 (let [loc (tree/string-zip "x")]
   (loc/nav loc [0])))

(defn trim-edges [loc {[from-path from-coords] :from
                       [to-path to-coords] :to} to-insert]
  (-> loc
      (loc/nav from-path)
      (replace-in-terminal! from-coords nil "")
      (loc/nav to-path)
      (replace-in-terminal! nil to-coords to-insert)))

(defn insert-from-text [loc kind s]
  (prn :insert kind)
  (let [path (loc/path loc)
        {pchildren :children} (z/node loc)
        {:keys [children]} (tree/ast s)
        [loc insertion-path] (case kind
                               :append-child [(reduce z/append-child loc children) (conj path 0)]
                               :prepend-child [(reduce z/insert-child loc (reverse children)) (conj path (count pchildren))]
                               :insert-before [(reduce z/insert-left loc children) path]
                               :insert-after [(reduce z/insert-right loc (reverse children)) path])]
    (delta/shift! insertion-path (count children))
    loc))

(defn inside-left [node inner-span offset]
  (and
   (coords/< offset (:from inner-span))
   (coords/> offset (:from node))))

(defn replacement-kind [loc offset]
  (let [node (z/node loc)
        inner-span (coords/inner-span node)
        end-pos (pointer/end-pos loc offset)
        kind (cond end-pos (case end-pos
                             :end-inner :append-child
                             :end-outer :insert-after)
                   (or (n/terminal? node)
                       (inside-left node inner-span offset)) :splice
                   (coords/= offset (:from node)) :insert-before
                   (coords/= offset (:from inner-span)) :prepend-child
                   :else
                   (throw (js/Error. (str "Do not know how to replace...`"
                                          (emit/string loc)
                                          "` " offset))))]
    kind))

(defn replace-span! [loc *span s]
  ;; TODO
  ;;
  ;; handle case where `to` is at the end of a coll.
  ;;
  (prn :REPLACE @*span s)
  (let [_ (prn 11 (emit/string loc))
        loc (trim-edges loc @*span s)
        _ (prn 12 (emit/string loc))
        ;_ (delta/cursor! [(first (:to span)) (string/end-coords s)])
        loc (remove-between! loc *span)
        _ (prn 13 (emit/string loc))
        loc (merge-between! loc *span)
        ]


    loc))

(defn replace-at-loc! [loc {:keys [from to]} s]
  (let [to (or to from)
        from-loc (loc/get-loc loc (first from))
        kind (replacement-kind from-loc (second to))]
    (case kind
      :splice (replace-in-terminal! from-loc (second from)
                                    (second to) s)
      (insert-from-text from-loc kind s))))

(defmethod impl/-operate :edit/replace
  [_ state [s]]
  (->> (delta/selections)
       (reduce
        (fn replace-span* [state *span]
          (let [loc (tree/zip (:ast state))
                loc (if (pointer/same-loc? @*span)
                      (replace-at-loc! loc @*span s)
                      (replace-span! loc *span s))]
            (when-let [to (:to @*span)]
              (reset! *span {:from to}))
            (assert loc (str "loc not returned"))
            (assoc state :ast (z/root loc))))
        state)))

(s/fdef replace-span!
        :args (s/cat :loc ::loc/loc
                     :span #(satisfies? IDeref %) #_::pointer/span
                     :s string?)
        :ret ::loc/loc)

(s/fdef replace-in-terminal!
        :args (s/cat :loc ::loc/loc
                     :start (s/nilable ::coords/coord)
                     :end (s/nilable ::coords/coord)
                     :text string?)
        :ret ::loc/loc)

(s/fdef replace-at-loc!
        :args (s/cat :loc ::loc/loc
                     :span ::pointer/span
                     :text string?)
        :ret ::loc/loc)