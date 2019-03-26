(ns lark.structure.loc
  (:require [fast-zip.core :as z]
            [lark.structure.delta :as delta]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.structure.string :as string]
            [lark.structure.coords :as coords]
            [cljs.spec.alpha :as s]
            [lark.structure.path :as path]))

(s/def ::loc #(instance? z/ZipperLocation %))

(defn get-loc
  "Returns `loc` at `path`"
  [loc path]
  (loop [segments (seq path)
         loc loc]
    (if (or (not loc)
            (not segments))
      loc
      (recur (next segments)
             (->> (iterate z/right (z/down loc))
                  (take-while identity)
                  (drop (first segments))
                  (first))))))

(defn path
  "Returns path to `loc` from root"
  [loc]
  (loop [loc loc
         out ()]
    (if-not loc
      (vec (drop 1 out))
      (recur (z/up loc)
             (cons (->> (z/lefts loc)
                        (count))
                   out)))))

(defn next-between
  "Returns loc s between `from-path` and `to-path`, inclusive of from and to."
  [loc to-path]
  (let [from-path (path loc)]
    (if (= from-path to-path)
      loc
      (if (path/ancestor? from-path to-path)
        (z/down loc)
        (or (z/right loc)
            (z/up loc)
            (throw (js/Error. "Could not find")))))))

(defn locs-between
  [loc to-path]
  (lazy-seq
   (when loc
     (cons loc
           (let [p (path loc)]
             (when (not= p to-path)
               (locs-between (next-between loc to-path) to-path)))))))

(defn remove-loc! [loc]
  (let [p (path loc)]
    (print "  " :remove-loc! p (emit/string loc))
    (delta/shift! p -1)
    (z/remove loc)))

(s/fdef remove-loc!
        :args (s/cat :loc ::loc)
        :ret ::loc)

(defn replace-splice! [loc nodes]
  (if (empty? nodes)
    (remove-loc! loc)
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
               (if (= ppath path)
                 (let [new-pointer
                       (reduce (fn [[path offset] [npath nstart]]
                                 (if (coords/> nstart poffset)
                                   (reduced [path (coords/+ offset)])
                                   [npath (coords/- offset nstart)]))
                               pointer
                               offsets)]
                   #_#_#_(prn :updated-pointer)
                       (print "  " {ppath poffset})
                       (print "  " (apply hash-map new-pointer))
                   new-pointer)
                 pointer)))
            loc))))))

(defn top [loc]
  (loop [loc loc]
    (if-let [up (z/up loc)]
      (recur up)
      loc)))

(defn- call-n
  "Calls `f` on `x` `n` times."
  [f n x]
  {:pre [(fn? f)
         (int? n)]}
  (let [n (max n 0)]
    (loop [i 0
           x x]
      (if (= n i)
        x
        (recur (inc i) (f x))))))

(defn nav
  [loc to-path]
  (let [[from to] (path/drop-common (path loc) to-path)
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

(defn to-child [loc i]
  (call-n z/right i (z/down loc)))


(defn joinable? [loc1 loc2]
  (and (some-> (z/right loc1)
               (z/node)
               (identical? (z/node loc2)))
       (let [t1 (.-tag (z/node loc1))
             t2 (.-tag (z/node loc2))]
         (or (= t1 t2)
             (every? #{:vector
                       :list} [t1 t2])))))

(defn join-terminals [loc1 loc2 p1 p2]
  (let [{v1 :value} (z/node loc1)
        {v2 :value} (z/node loc2)
        loc (-> (remove-loc! loc2)
                (nav p1)
                (z/edit assoc :value (str v1 v2)))]
    (delta/move-offsets! p2 p1 (string/end-coords v1))
    #_(delta/add-offset! p1 (string/end-coords v1))
    loc))

(defn join-colls! [loc1 loc2 p1 p2]
  (let [{c1 :children} (z/node loc1)
        {c2 :children} (z/node loc2)]
    (delta/move-coll-path! p2 p1 (count c1))
    (let [loc (-> loc2
                  (remove-loc!)
                  (nav p1)
                  (z/edit assoc :children (into (vec c1) c2)))
          outloc (cond-> loc
                         (seq c2)
                         (to-child (count c1)))]
      outloc)))

(defn join! [loc1 loc2]
  (let [p1 (path loc1)
        p2 (path loc2)
        colls? (n/may-contain-children? (z/node loc1))
        joined (if colls?
                 (join-colls! loc1 loc2 p1 p2)
                 (join-terminals loc1 loc2 p1 p2))]
    joined))


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

