(ns lark.cards.structure.loc
  (:require [fast-zip.core :as z]
            [lark.cards.structure.delta :as delta]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.cards.structure.string :as string]
            [lark.cards.structure.coords :as coords]
            [cljs.spec.alpha :as s]
            [lark.tree.core :as tree]
            [applied-science.js-interop :as j]
            [lark.tree.nav :as nav]
            [lark.cards.structure.path :as path]))


(s/def ::loc #(instance? z/ZipperLocation %))

(defn get-loc
  "Returns `loc` at `path`"
  [loc path]
  (loop [segments path
         loc loc]
    (if (or (not loc)
            (empty? segments))
      loc
      (recur (rest segments)
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
      (drop 1 out)
      (recur (z/up loc)
             (conj out (->> (z/lefts loc)
                            (count)))))))

(defn replace-splice [loc {:as node
                           :keys [children]}]
  (let [delta (dec (count children))]
    (when-not (zero? delta)
      (delta/shift! (path/parent (path loc)) delta)))
  (if (empty? children)
    (z/remove loc)
    (let [[c & children] children]
      (loop [loc (z/replace loc c)
             children children]
        (if children
          (recur (-> (z/insert-right loc (first children))
                     (z/right))
                 (next children))
          loc)))))

(defn next-between
  "Returns loc s between `from-path` and `to-path`, inclusive of from and to."
  [loc to-path]
  (let [p (path loc)]
    (if (= p to-path)
      loc
      (let [from-path (path loc)]
        (if (path/contains? from-path to-path)
          (z/down loc)
          (or (z/right loc)
              (z/up loc)
              (throw (js/Error. "Could not find"))))))))

(defn locs-between
  [loc to-path]
  (lazy-seq
   (when loc
     (cons loc
           (let [p (path loc)]
             (when (not= p to-path)
               (locs-between (next-between loc to-path) to-path)))))))

(defn remove-loc [loc]
  (let [p (path loc)]
    (delta/add! {:shifts (delta/shift (path/parent p) -1)})
    (z/remove loc)))

(defn joinable? [loc1 loc2]
  (and (some-> (z/right loc1)
               (z/node)
               (identical? (z/node loc2)))
       (let [t1 (.-tag (z/node loc1))
             t2 (.-tag (z/node loc2))
             joinable (or (= t1 t2)
                          (every? #{:vector
                                    :list} [t1 t2]))]
         (when joinable
           (prn [t1 t2] (mapv emit/string [loc1 loc2])))
         joinable)))

(defn join-terminals [loc1 loc2 p1 p2]
  (let [{v1 :value} (z/node loc1)
        {v2 :value} (z/node loc2)
        loc (-> loc2
                (z/remove)
                (z/edit assoc :value (str v1 v2)))]
    (delta/set-offset! p1 (string/end-coords v1))
    loc))

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


(defn join-colls [loc1 loc2 p1 p2]
  (let [{:as node1
         c1 :children} (z/node loc1)
        {:as node2
         c2 :children} (z/node loc2)]

    (delta/shift! p1 (count c1))                            ;; children of p2 are now in p1, shift to the right by # of children in p1

    (let [joined-loc (z/edit loc1 assoc :children (into (vec c1) c2))
          dest-loc (if (seq c2)
                     ;; return loc of 1st joined child
                     #_(call-n z/right (count c1) (z/down joined-loc))
                     (first (drop (count c1) (iterate z/next (z/down joined-loc))))
                     joined-loc)
          dest-path (path dest-loc)]
      (-> dest-loc
          (top)
          (get-loc p2)
          (z/remove)
          (nav dest-path)))))

(defn join [loc1 loc2]
  (let [p1 (path loc1)
        p2 (path loc2)
        joined (if (n/may-contain-children? (z/node loc1))
                 (join-colls loc1 loc2 p1 p2)
                 (join-terminals loc1 loc2 p1 p2))]
    ;; parent of p1 has one less child    [a][b] => [ab]
    (delta/shift! (path/parent p1) -1)
    (prn :join (mapv emit/string [loc1 loc2])
         :return (emit/string joined))
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