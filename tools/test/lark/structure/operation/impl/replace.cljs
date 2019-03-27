(ns lark.structure.operation.impl.replace
  (:require [lark.structure.path :as path]
            [fast-zip.core :as z]
            [lark.structure.loc :as loc]
            [lark.tree.core :as tree]
            [lark.structure.delta :as delta]
            [lark.tree.emit :as emit]
            [lark.structure.string :as string]
            [lark.tree.parse :as parse]
            [chia.util :as u]
            [lark.structure.coords :as coords]
            [lark.structure.pointer :as pointer]
            [cljs.spec.alpha :as s]
            [lark.tree.node :as n]))

(def debug? false)

(defn print! [& args]
  (when debug?
    (apply print args)))

(defn replace-in-terminal! [loc {from-coords :from
                                 to-coords :to} text]
  (assert (n/terminal? (z/node loc)))
  (print! "\n\nReplace in Terminal " (pr-str (emit/string loc)) (pr-str text))
  (delta/print-selections)
  (let [text (or text "")
        text-before (emit/string (z/node loc))
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
              (loc/replace-splice loc nodes-after)
              (loc/remove-loc loc))
        offset (-> from-coords
                   (coords/- to-coords)
                   (coords/+ (string/end-coords text)))]
    (delta/add-offset! (:from @insertion-point) offset)
    (delta/print-selections)
    loc))

(defn merge-between [loc span]
  (assert loc "merge-between requires loc")
  (print! "\n\nMerge Between" (emit/string (loc/top loc)) ", " (first (:from @span)) ">" (first (:to @span)))
  (let [[spath sloc :as start] (let [p (first (:from @span))]
                                 [p (loc/nav loc p)])]
    (if-not sloc
      (do
        (prn :merge-between-no-loc)
        loc)
      (loop [ploc nil
             [p loc] start]
        (let [to (first (:to @span))
              action (cond (= p to) :return
                           (loc/joinable? ploc loc) :join
                           (path/> p to) :too-far
                           :else :continue)]

          (print! "  " action (some-> ploc (emit/string) (pr-str)) (some-> loc (emit/string) (pr-str)) p "TO: " to)

          (case action
            :return loc
            :join (recur nil [(loc/path ploc) (loc/into-loc ploc loc)])
            :continue (recur loc (loc/next-between [p loc] to))
            :too-far (throw (js/Error. "Navigated too far"))))))))



(defn remove-between! [loc span]
  (let [start-p (first (:from @span))
        start-loc (loc/nav loc start-p)]
    (print! "\n\nRemove Between")
    (loop [[path loc] [start-p start-loc]
           i 0]
      (let [{[from _] :from
             [to _] :to} @span
            action (cond (= path to) :return
                         (= path from) :next
                         (= :end (path/get-last path)) :next
                         (or (path/ancestor? path from)
                             (path/ancestor? path to)) :next
                         :else :remove)
            ]
        (when (> i 100) (throw (js/Error. "Looping")))
        (print! "  " action (pr-str (emit/string loc)))
        (case action
          :return loc
          :next (recur (loc/next-between [path loc] to) (inc i))
          :remove (let [loc (loc/remove-loc loc)]
                    (recur [(loc/path loc) loc] (inc i))))))))

(comment
 (let [loc (tree/string-zip "x")]
   (loc/nav loc [0])))

(defn insert-by-text [loc kind s]
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

(defn cut-edge [loc side span text-insert]
  (let [[path coords] (get @span side)
        loc (loc/nav loc path)]
    (cond (= :end (path/get-last path))
          (cond-> loc
                  text-insert (insert-by-text :append-child text-insert))

          (n/terminal? (z/node loc))
          (replace-in-terminal! loc {side coords} text-insert)

          :else
          (if text-insert
            (loc/replace-splice loc (:children (tree/ast text-insert)))
            (loc/remove-loc loc)))))

(defn inside-left [node inner-span offset]
  (and
   (coords/< offset (:from inner-span))
   (coords/> offset (:from node))))

(defn replace-span! [loc *span s]
  (-> loc
      (remove-between! *span)
      (cut-edge :from *span nil)
      (cut-edge :to *span s)
      (merge-between *span)))

(defn replacement-kind [loc [path offset]]
  (let [node (z/node loc)
        inner-span (coords/inner-span node)
        end-pos (pointer/end-pos? path)
        kind (cond end-pos :append-child
                   (or (n/terminal? node)
                       (inside-left node inner-span offset)) :splice
                   (coords/= offset (:from node)) :insert-before
                   (coords/= offset (:from inner-span)) :prepend-child
                   :else
                   (throw (js/Error. (str "Do not know how to replace...`"
                                          (emit/string loc)
                                          "` " offset))))]
    kind))

(defn replace-at-path! [loc {:keys [from to]} s]
  (let [path (first from)
        loc (loc/nav loc path)
        kind (replacement-kind loc from)]
    (case kind
      :splice (let [from-offset (second from)
                    to-offset (if to (second to) from-offset)]
                (replace-in-terminal! loc {:from from-offset
                                           :to to-offset} s))
      (insert-by-text loc kind s))))

(s/fdef replace-span!
        :args (s/cat :loc ::loc/loc
                     :span #(satisfies? IDeref %) #_::pointer/span
                     :s string?)
        :ret ::loc/loc)

(s/fdef replace-in-terminal!
        :args (s/cat :loc ::loc/loc
                     :span ::coords/span
                     :text (s/nilable string?))
        :ret ::loc/loc)

(s/fdef replace-at-path!
        :args (s/cat :loc ::loc/loc
                     :span ::pointer/span
                     :text string?)
        :ret ::loc/loc)

(s/fdef replacement-kind
        :args (s/cat :loc ::loc/loc
                     :pointer ::pointer/pointer))


(comment
 (-> (delta/track []
                  (let [loc (tree/string-zip "[a][b]")
                        joined (loc/into-loc (z/down loc)
                                             (-> loc z/down z/right))]
                    {:ast (z/root joined)}))
     :ast
     emit/string))