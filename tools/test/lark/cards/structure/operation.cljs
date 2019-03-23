(ns lark.cards.structure.operation
  (:require [lark.cards.structure.string :as string]
            [lark.cards.structure.coords :as coords]
            [lark.cards.structure.loc :as loc]
            [lark.cards.structure.path :as path]
            [lark.cards.structure.pointer :as pointer]
            [lark.tree.core :as tree]
            [fast-zip.core :as z]
            [lark.tree.emit :as emit]
            [lark.tree.parse :as parse]
            [lark.cards.structure.delta :as delta]
            [lark.tree.range :as range]
            [chia.util :as u]
            [lark.tree.node :as n]
            [lark.cards.structure.position :as pos]
            [lark.tree.reader :as rd]
            [cljs.spec.alpha :as s]))

(defmulti -operate identity)

(defmethod -operate :default
  [op state]
  (js/console.warn (str "Operation not implemented: " op))
  state)

(defn operate [op state & args]
  (let [state (-> (delta/track-deltas
                   (apply -operate op state args))
                  (update :selections #(into [] (comp (map pos/normalize-range)
                                                      (distinct)) %)))]
    (assert (instance? rd/Node (:ast state))
            (str "did not return an AST: " op args (:ast state)))
    state))

(defn insert-text* [loc from-coords to-coords text]
  (let [text-before (emit/string (z/node loc))
        end-coords (or to-coords (string/end-coords text-before))
        text-after (string/replace-at-coords text-before
                                             {:from from-coords
                                              :to end-coords}
                                             text)
        loc (if (u/some-str text-after)
              (-> text-after
                  (parse/ast)
                  (->> (loc/replace-splice loc))
                  #_(->> (z/replace loc)))
              (loc/remove-loc loc))
        offset (-> from-coords
                   (coords/- end-coords)
                   (coords/+ (string/end-coords text)))
        path (loc/path loc)]
    (delta/offset! path offset)
    loc))

(s/fdef insert-text*
        :args (s/cat :loc ::loc/loc
                     :start ::coords/coord
                     :end (s/nilable ::coords/coord)
                     :text string?))

(defn replace-span [state {:keys [from to]} s]
  (let [ast
        (loop [loc0 (loc/get-loc (tree/ast-zip (:ast state))
                                 (first (pointer/resolve from)))
               i 0]
          (when (> i 100)
            (throw (js/Error. "Stuck in loop" (emit/string loc0))))
          (let [[$from-path $from-offset] (pointer/resolve from)
                [$to-path $to-offset] (pointer/resolve to)
                p0 (loc/path loc0)
                start? (= p0 $from-path)
                end? (= p0 $to-path)
                remove? (and (not= p0 $from-path)
                             (not= p0 $to-path)
                             (not (path/contains? p0 $from-path))
                             (not (path/contains? p0 $to-path)))
                loc1 (cond (or start? end?) (let [s (if (= p0 $to-path)
                                                      s
                                                      "")
                                                  from (cond (= p0 $from-path) $from-offset
                                                             (= p0 $to-path) (or (delta/get-offset $to-path) [0 0]))
                                                  to (cond (= p0 $to-path) $to-offset)]
                                              (insert-text* loc0 from to s))

                           remove? (loc/remove-loc loc0)
                           :else loc0)
                path1 (loc/path loc1)
                loc2 (loc/next-between loc1 $to-path)]


            (prn 1 (emit/string loc0))
            (when remove?
              (prn :remove! p0 $from-path $to-path))
            (prn 2 (emit/string loc1))
            (when (loc/joinable? loc1 loc2)
              (prn :join-with (emit/string loc2)))
            (prn :start? start?
                 :end? (= p0 $to-path))
            (print "\n\n")

            (if (= path1 $to-path)
              (z/root loc1)
              (if loc2
                (if (and #_(path/contains? path1 $from-path)
                     (loc/joinable? loc1 loc2))
                  (recur (loc/join loc1 loc2) (inc i))
                  (recur loc2 (inc i)))
                (do
                  (throw (js/Error. "Next loc not found")))))))]

    (delta/selections! [{:from (pointer/resolve to)}])      ;; set end selection
    (assoc state :ast ast)))

(defmethod -operate :edit/replace
  [_ state [s]]
  (delta/track-selections
   (->> (:selections state)
        (reduce
         (fn replace-span* [state pointer-span]
           (let [{:as $selection
                  :keys [from to]} (pointer/resolve-span pointer-span)
                 to (or to from)]
             (if (and (= (first from) (first to)))
               ;; single node modified
               (let [from-loc (-> (tree/ast-zip (:ast state))
                                  (loc/get-loc (first from)))
                     modified-loc (insert-text* from-loc
                                                (second from)
                                                (second to)
                                                s)]
                 (delta/selections! [{:from (update from 1 coords/+ (string/end-coords s))}])
                 (assoc state :ast (z/root modified-loc)))
               (replace-span state pointer-span s))))
         state))))

(defmethod -operate :identity
  [_ state]
  state)

(defn- move-path [loc path axis distance]
  (->> (update (pointer/position loc path)
               (case axis :x :column
                          :y :line)
               +
               distance)
       (pos/pointer loc)))

(defn- move-selection [loc [axis distance] {:as selection
                                            :keys [from to]}]
  (let [path (case axis
               :x (if (range/point? selection)
                    (move-path loc from axis distance)
                    (if (pos? distance)
                      to
                      from))
               :y (let [path (or (if (pos? distance)
                                   to
                                   from)
                                 from)]
                    (move-path loc path :y distance)))]
    {:from path}))

(defmethod -operate :cursor/move
  [_ {:as state
      :keys [ast]} [axis distance]]
  (if (zero? distance)
    state
    (let [loc (tree/ast-zip ast)]
      (assoc state :selections (mapv (partial move-selection loc [axis distance])
                                     (:selections state))))))

#_(defmethod operate :cursor/hop
    [_ {:as state
        :keys [ast
               selections]} direction]
    (let [loc (tree/ast-zip ast)]
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

