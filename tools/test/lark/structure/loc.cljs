(ns lark.structure.loc
  (:require [fast-zip.core :as z]
            [lark.structure.delta :as delta]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.structure.string :as string]
            [lark.structure.coords :as coords]
            [cljs.spec.alpha :as s]
            [lark.structure.path :as path]
            [lark.tree.core :as tree]
            [applied-science.js-interop :as j]))

(defn- call-n
  "Calls `f` on `x` `n` times."
  [f n x]
  {:pre [(fn? f)
         (int? n)]}
  (let [n (max n 0)]
    (loop [i 0
           x x]
      (if (identical? n i)
        x
        (recur (inc i) (f x))))))

(defn path
  "Returns path to `loc` from root"
  [loc]
  (loop [^z/ZipperPath zpath (.-path loc)
         path #js[]]
    (if-let [ppath (and zpath ^z/ZipperPath (.-ppath zpath))]
      (recur ppath (j/push! path (count (.-l zpath))))
      (vec (.reverse (cond-> path
                             zpath (j/push! (count (.-l zpath)))))))))

(defn nav
  "Moves `loc` to `to-path`"
  [loc to-path]
  (let [to-path (cond-> to-path
                        (keyword-identical? :end (path/get-last to-path)) (pop))
        [from to] (path/drop-common (path loc) to-path)
        loc (call-n z/up (dec (count from)) loc)
        from (seq (take 1 from))]
    (loop [from from
           to to
           loc loc]
      (cond from
            (if (seq to)
              (case (compare (first from)
                             (first to))
                -1 (recur (next from)
                          (next to)
                          (call-n z/right (- (first to)
                                             (first from)) loc))
                1 (recur (next from)
                         (next to)
                         (call-n z/left (- (first from)
                                           (first to)) loc)))
              (recur (next from)
                     to
                     (z/up loc)))
            (seq to)
            (recur from
                   (drop 1 to)
                   (->> (z/down loc)
                        (call-n z/right (first to))))
            :else loc))))

(defn get-loc
  "Returns `loc` nested in `path`"
  [loc path]
  (loop [segments (seq path)
         loc loc]
    (let [nsegment (first segments)]
      (if (or (not loc)
              (not segments)
              (keyword-identical? :end nsegment))
        loc
        (recur (next segments)
               (->> (iterate z/right (z/down loc))
                    (take-while identity)
                    (drop nsegment)
                    (first)))))))

(comment
 (let [z (tree/string-zip "[a[ d [ a b [a b ] ]  e]]")
       p [0 1 3 5 2]
       loc (get-loc z p)
       dc (fn [loc to-path]
            (path/drop-common (path loc) to-path))
       gp (fn [loc to-path]
            (path loc))

       _p1 (into [] p)
       _p2 (into [] p)]

   (simple-benchmark [g =] (g _p1 _p2) 100000)
   (simple-benchmark [g path/equal] (g _p1 _p2) 100000)

   (= (= _p1 _p2)
      (path/equal _p1 _p2))))

;; NOTE
;; we will need to iterate across _locs and paths_, not only locs.
(defn next-path-between
  "Return next path between `from-path` and `to-path`, inclusive of from and to."
  [loc from-path to-path]
  (assert (not (path/before? to-path from-path)))
  (if (path/equal from-path to-path)
    to-path
    (if (path/ancestor? from-path to-path)
      (if (z/down loc) (conj from-path 0)
                       (conj from-path :end))
      (if (keyword-identical? :end (path/get-last from-path))
        (pop from-path)
        (if (z/right loc)
          (path/update-last from-path inc)
          (path/update-last from-path (constantly :end)))))))

(defn next-between [[from-path from-loc] to-path]
  (let [p (next-path-between from-loc from-path to-path)]
    [p (nav from-loc p)]))

(defn paths-between
  [loc from-path to-path]
  (lazy-seq
   (when loc
     (cons from-path
           (when (not (path/equal from-path to-path))
             (let [npath (next-path-between loc from-path to-path)]
               (paths-between (nav loc npath) npath to-path)))))))

(defn remove-loc [loc]
  (let [p (path loc)]
    ;(print "  " :remove-loc! p (emit/string loc))
    (delta/shift! p -1)
    (z/remove loc)))

(s/def ::loc #(instance? z/ZipperLocation %))

(defn replace-splice [loc nodes]
  ;; MAYBE,
  ;; refactor to receive the splice-path, to support :end.
  (if (empty? nodes)
    (remove-loc loc)
    (let [path (path loc)
          parent (path/parent path)]
      (loop [loc (z/replace loc (first nodes))
             i 1
             offset-so-far [0 0]
             offsets [[path [0 0] (emit/string loc)]]
             children (next nodes)
             spliced []]
        (if children
          (let [c (first children)
                text (emit/string c)
                offset-so-far (coords/+ offset-so-far (string/end-coords text))]
            (recur (-> (z/insert-right loc c)
                       (z/right))
                   (inc i)
                   offset-so-far
                   (conj offsets [(path/append parent i) offset-so-far])
                   (next children)
                   (conj spliced (first children))))
          (do
            (delta/update-pointers!
             (fn [[ppath poffset :as pointer]]
               (if (path/equal ppath path)
                 (reduce (fn [[path offset] [npath nstart]]
                           (if (coords/> nstart poffset)
                             (reduced [path (coords/+ offset)])
                             [npath (coords/- offset nstart)]))
                         pointer
                         offsets)
                 pointer)))
            loc))))))

(defn top [loc]
  (loop [loc loc]
    (if-let [up (z/up loc)]
      (recur up)
      loc)))


(defn to-child [loc i]
  (call-n z/right i (z/down loc)))

(defn joinable? [loc1 loc2]
  (and loc1 loc2
       (some-> (z/right loc1)
               (z/node)
               (identical? (z/node loc2)))
       (let [t1 (.-tag (z/node loc1))
             t2 (.-tag (z/node loc2))]
         (or (keyword-identical? t1 t2)
             (every? #{:vector
                       :list} [t1 t2])))))

(defn append-terminal [loc1 loc2 p1 p2]
  (let [{v1 :value} (z/node loc1)
        {v2 :value} (z/node loc2)
        loc (-> (remove-loc loc2)
                (nav p1)
                (z/edit assoc :value (str v1 v2)))]
    (delta/move-offsets! p2 p1 (string/end-coords v1))
    #_(delta/add-offset! p1 (string/end-coords v1))
    loc))

(defn append-coll [loc1 loc2 p1 p2]
  (let [{c1 :children} (z/node loc1)
        {c2 :children} (z/node loc2)]
    (delta/move-coll-path! p2 p1 (count c1))
    (let [loc (-> loc2
                  (remove-loc)
                  (nav p1)
                  (z/edit assoc :children (into (vec c1) c2)))
          outloc (cond-> loc
                         (seq c2)
                         (to-child (count c1)))]
      outloc)))

(defn into-loc
  "Copies content of `loc2` into `loc1`, removes loc2, returns loc1"
  [loc1 loc2]
  (let [p1 (path loc1)
        p2 (path loc2)
        colls? (n/may-contain-children? (z/node loc1))
        joined (if colls?
                 (append-coll loc1 loc2 p1 p2)
                 (append-terminal loc1 loc2 p1 p2))]
    (nav joined p1)))



(s/fdef remove-loc
        :args (s/cat :loc ::loc)
        :ret ::loc)

(comment
 (defn ops-between
   [from to]
   (let [[from to] (path/drop-common from to)
         ops (vec (take (dec (count from)) (repeat :up)))
         from (seq (take 1 from))]
     (loop [from from
            to to
            ops ops]
       (cond from
             (if (seq to)
               (case (compare (first from)
                              (first to))
                 -1 (recur (next from)
                           (next to)
                           (into ops (take (- (first to)
                                              (first from)) (repeat :right))))
                 1 (recur (next from)
                          (next to)
                          (into ops (take (- (first from)
                                             (first to)) (repeat :left)))))
               (recur (next from)
                      to
                      (conj ops :up)))
             (seq to)
             (recur from
                    (drop 1 to)
                    (-> ops
                        (conj :down)
                        (into (take (first to) (repeat :right)))))
             :else ops))))

 (for [[from to expected] (partition 3 [

                                        [] [0] [:down]
                                        [] [0 0] [:down :down]
                                        [] [0 0 0] [:down :down :down]
                                        [] [0 0 1] [:down :down :down :right]

                                        [] [1] [:down :right]
                                        [] [1 1] [:down :right :down :right]

                                        [0] [] [:up]
                                        [0 0] [] [:up :up]
                                        [1 1] [] [:up :up]
                                        [1 1 1] [] [:up :up :up]

                                        [0] [0] []
                                        [0] [1] [:right]
                                        [1] [0] [:left]

                                        [0 0] [1] [:up :right]
                                        [0 0] [1 1] [:up :right :down :right]

                                        [0 1] [0 2] [:right]

                                        [0 1] [0 2 2] [:right :down :right :right]

                                        [] [1 2 3] [:down :right :down :right :right :down :right :right :right]
                                        [1 2 3] [2] [:up :up :right]

                                        ])
       :let [ops (ops-between from to)]
       :when (not= expected ops)]
   [:from from :to to :expected expected :actual ops]))


(comment
 (let [loc (tree/string-zip "()")
       from []
       to [:end]
       loc (get-loc loc from)]
   (paths-between loc from to)))