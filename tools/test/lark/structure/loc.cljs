(ns lark.structure.loc
  (:require [lark.fast-zip :as z]
            [lark.structure.delta :as delta]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.structure.string :as string]
            [lark.structure.coords :as coords]
            [cljs.spec.alpha :as s]
            [lark.structure.path :as path]
            [lark.tree.core :as tree]
            [applied-science.js-interop :as j]
            [cljs.pprint :as pp]))

(defn prev-lateral
  "Return next loc moving toward `to-path`, only enters children of locs when necessary."
  ([loc]
   (prev-lateral loc []))
  ([loc to-path]
   (let [p (z/path loc)]
     (if (path/= p to-path)
       loc
       (if (path/ancestor? p to-path)
         (z/as-sentinel loc)
         (if (path/sentinel? p)
           (if (seq (z/children loc))
             (z/nav loc
                    (path/assoc-last p (dec (z/lefts-count loc))))
             (z/drop-sentinel loc))
           (or (z/left loc)
               (z/up loc))))))))

(defn next-lateral
  "Return prev loc moving toward `to-path`, only enters children of locs when necessary."
  ([loc]
   (next-lateral loc [:end]))
  ([loc to-path]
   (let [p (z/path loc)]
     (if (path/= p to-path)
       loc
       (if (path/ancestor? p to-path)
         (if (seq (z/children loc))
           (z/down loc)
           (z/as-sentinel loc))
         (if (path/sentinel? p)
           (z/drop-sentinel loc)
           (or (z/right loc)
               (z/as-sentinel (z/up loc)))))))))

(defn remove-loc [loc]
  (let [p (z/path loc)
        prevp (z/path (prev-lateral loc))]
    (if (z/only-child? loc)
      (delta/shift-children-up! (path/parent p) :remove-loc)
      (delta/shift! p -1 :remove-loc))
    (do
      (prn :remove-loc p prevp)
      (-> (z/remove loc)
          (z/nav prevp)))))

(s/def ::loc #(instance? z/ZipperLocation %))

(defn replace-splice [loc nodes]
  (let [ppath (z/path (prev-lateral loc))
        loc
        (if (empty? nodes)
          (remove-loc loc)
          (let [path (z/path loc)
                parent (path/parent path)
                i (path/last path)]
            (let [delta-here (string/end-coords (emit/string (first nodes)))]
              (loop [loc (z/replace loc (first nodes))
                     offset-acc delta-here
                     offset-index [[i delta-here delta-here]]
                     children (next nodes)
                     i (inc i)]
                (if children
                  (let [c (first children)
                        text (emit/string c)
                        delta-here (string/end-coords text)]
                    (recur (-> (z/insert-right loc c)
                               (z/right))
                           (coords/+ offset-acc delta-here)
                           (conj offset-index [i delta-here (coords/+ offset-acc delta-here)])
                           (next children)
                           (inc i)))
                  (do
                    (delta/update-pointers!
                     (fn [[pointer-path pointer-offset :as pointer]]
                       (if (path/= pointer-path path)
                         (->> offset-index
                              (reduce (fn [offset [i n-delta n-total :as o]]
                                        (if (coords/>= n-total pointer-offset)
                                          (reduced [(path/append parent i) offset])
                                          (coords/- offset n-delta)))
                                      (second pointer)))
                         pointer)) :replace-splice)
                    loc))))))]
    (z/nav loc ppath)))

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
                (z/nav p1)
                (z/edit assoc :value (str v1 v2)))]
    (delta/move-offsets! p2 p1 (string/end-coords v1) :append-terminal)
    #_(delta/add-offset! p1 (string/end-coords v1))
    loc))

(defn append-coll [loc1 loc2 p1 p2]
  (let [{c1 :children} (z/node loc1)
        {c2 :children} (z/node loc2)]
    (delta/move-coll-path! p2 p1 (count c1) :append-coll)
    (let [loc (-> loc2
                  (remove-loc)
                  (z/nav p1)
                  (z/edit assoc :children (into (vec c1) c2)))
          outloc (cond-> loc
                         (seq c2)
                         (z/to-child (count c1)))]
      outloc)))

(defn into-loc
  "Copies content of `loc2` into `loc1`, removes loc2, returns loc1"
  [loc1 loc2]
  (let [p1 (z/path loc1)
        p2 (z/path loc2)
        colls? (n/may-contain-children? (z/node loc1))
        joined (if colls?
                 (append-coll loc1 loc2 p1 p2)
                 (append-terminal loc1 loc2 p1 p2))]
    (z/nav joined p1)))



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
 (let [z (tree/string-zip "[a[ d [ a b [a b ] ]  e]]")
       p [0 1 3 5 2]
       loc (get-loc z p)
       dc (fn [loc to-path]
            (path/drop-common (z/path loc) to-path))
       gp (fn [loc to-path]
            (z/path loc))

       _p1 (into [] p)
       _p2 (into [] p)]

   (simple-benchmark [g =] (g _p1 _p2) 100000)
   (simple-benchmark [g path/=] (g _p1 _p2) 100000)

   (= (= _p1 _p2)
      (path/= _p1 _p2))))