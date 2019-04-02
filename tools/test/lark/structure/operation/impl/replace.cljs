(ns lark.structure.operation.impl.replace
  (:require [lark.structure.path :as path]
            [lark.fast-zip :as z]
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
  (print! "\n\nReplace in Terminal " (pr-str (emit/string loc)) (pr-str text) {:from from-coords :to to-coords})
  (delta/print-selections)
  (let [text (or text "")
        text-before (emit/string (z/node loc))
        from-coords (or from-coords [0 0])
        to-coords (or to-coords (string/end-coords text-before))
        path (z/path loc)
        insertion-point (delta/tracked {:from [path to-coords]})
        text-after (string/replace-at-coords text-before
                                             {:from from-coords
                                              :to to-coords}
                                             text)
        offset (-> from-coords
                   (coords/- to-coords)
                   (coords/+ (string/end-coords text)))
        nodes-after (:children (parse/ast text-after))
        ;_ (print "from-coords: " from-coords)
        ;_ (print "to-coords:   " to-coords)
        ;_ (print "string-coor: " (string/end-coords text))
        ;_ (print "offset..." (:from @insertion-point) "..." offset)
        _ (delta/add-offset! (:from @insertion-point) offset :replace-in-terminal)
        loc (loc/replace-splice loc nodes-after)]
    (delta/print-selections)
    loc))

(defn merge-between [loc pointer-span]
  (assert loc "merge-between requires loc")
  (print! "\n\nMerge Between" (emit/string (z/top loc)) ", " (first (:from @pointer-span)) ">" (first (:to @pointer-span)))
  (let [[spath sloc :as start] (let [p (first (:from @pointer-span))]
                                 [p (z/nav loc p)])]
    (when-not (= (z/path sloc) spath)
      (let [p (first (:from @pointer-span))]
        (prn :p p :path (z/path (z/nav loc p)) (prn :thing (emit/string (z/top loc))))))
    (assert (= (z/path sloc) spath)
            (str "p and loc should have same path"))
    (if-not sloc
      (do
        (throw (js/Error. ":merge-between-no-loc"))
        (prn :merge-between-no-loc)
        loc)
      (loop [ploc nil
             loc sloc
             i 0]
        (let [p (z/path loc)
              to (first (:to @pointer-span))
              action (cond (= p to) :return
                           (loc/joinable? ploc loc) :join
                           (path/> p to) :too-far
                           :else :continue)]

          (print! "  " action (some-> ploc (emit/string) (pr-str)) (some-> loc (emit/string) (pr-str)) p "TO: " to)

          (case action
            :return loc
            :join (recur nil (loc/into-loc ploc loc) (inc i))
            :continue (recur loc (loc/next-lateral loc to) (inc i))
            :too-far (throw (js/Error. "Navigated too far"))))))))

(comment
 (let [loc (tree/string-zip "x")]
   (z/nav loc [0])))

(defn insert-by-text [loc path kind s]
  (print! "insert-by-text" kind (prn-str s))
  (let [[loc path kind] (if (path/sentinel? path)
                          [(z/drop-sentinel loc) (path/parent path) :append-child]
                          [loc path kind])
        {pchildren :children} (z/node loc)
        {:keys [children]} (tree/ast s)
        _ (prn :insert-by-text path (z/sentinel? loc) kind)
        [loc insertion-path] (case kind
                               :append-child [(reduce z/append-child loc children) (conj path 0)]
                               :prepend-child [(reduce z/insert-child loc (reverse children)) (conj path (count pchildren))]
                               :insert-before [(reduce z/insert-left loc children) path]
                               :insert-after [(reduce z/insert-right loc (reverse children)) path])]
    (delta/shift! insertion-path (count children) :insert-by-text)
    loc))

(defn inside-left [node inner-span offset]
  (and
   (coords/< offset (:from inner-span))
   (coords/> offset (:from node))))

(defn remove-until! [start-loc to]
  (let [from (z/path start-loc)
        to* (delta/tracked {:to [to [0 0]]})]
    (print! "\n\nRemove Between" (emit/string start-loc) :f from :t to)
    (assert start-loc)
    (when-not start-loc
      (prn :LOC (emit/string start-loc)
           :PATH from
           :NOT_FOUND))
    (loop [loc start-loc
           i 0]
      (let [path (z/path loc) to (first (:to @to*))
            [action reason] (cond (path/>= path to) [:return :path-at-or-beyond-destination]
                                  (path/<= path from) [:next :at-beginning]
                                  (path/sentinel? path) [:next :sentinel]
                                  (or (path/ancestor? path from)
                                      (path/ancestor? path to)) [:next :ancestor]
                                  :else [:remove :else])
            ]
        (print action reason path)
        (when (> i 100) (throw (js/Error. "Looping")))
        (assert loc)
        (print! "  " action (pr-str (emit/string loc)))
        (case action
          :return loc
          :next (if-let [loc (loc/next-lateral loc to)]
                  (recur loc (inc i))
                  loc)
          :remove (recur (loc/remove-loc loc) (inc i)))))))

(defn remove-between! [loc pointer-span]
  (let [{[from _] :from
         [to _] :to} @pointer-span]
    (remove-until! (z/nav loc from) to)))

(defn cut-edge [loc side span text-insert]

  (let [[path coords] (get @span side)
        loc (z/nav loc path)]
    (print! "Cut-Edge" side (prn-str text-insert) (prn-str path))
    (assert loc "cut-edge requires a loc")
    (cond (path/sentinel? path)
          (cond-> loc
                  text-insert (insert-by-text path :insert-before-sentinel text-insert))

          (n/terminal? (z/node loc))
          (replace-in-terminal! loc {side coords} text-insert)

          :else
          (if text-insert
            (loc/replace-splice loc (:children (tree/ast text-insert)))
            (loc/remove-loc loc)))))

(defn replace-span! [loc *span s]
  (delta/print-selections)
  (let [
        loc (cut-edge loc :from *span nil)

        _ (delta/print-selections)
        loc (remove-until! loc (first (:to @*span)))
        _ (delta/print-selections)
        loc (cut-edge loc :to *span s)
        _ (delta/print-selections)
        loc (merge-between loc *span)]
    (delta/print-selections)
    loc))

(defn replacement-kind [loc [path offset]]
  (let [node (z/node loc)
        inner-span (coords/inner-span node)
        sentinel (path/sentinel? path)
        kind (cond sentinel :insert-before-sentinel
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
        _ (prn :path path :loc loc (emit/string loc))
        loc (z/nav loc path)
        _ (assert loc "must have loc")
        kind (replacement-kind loc from)]
    (case kind
      :splice (let [from-offset (second from)
                    to-offset (if to (second to) from-offset)]
                (replace-in-terminal! loc {:from from-offset
                                           :to to-offset} s))
      (insert-by-text loc path kind s))))

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

(s/fdef remove-between!
        :args (s/cat :loc ::loc/loc
                     :span ::pointer/span))

(s/fdef merge-between
        :args (s/cat :loc ::loc/loc
                     :span ::pointer/span))

(comment
 (-> (delta/track []
       (let [loc (tree/string-zip "[a][b]")
             joined (loc/into-loc (z/down loc)
                                  (-> loc z/down z/right))]
         {:ast (z/root joined)}))
     :ast
     emit/string))