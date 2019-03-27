(ns lark.structure.pointer
  (:refer-clojure :exclude [resolve])
  (:require [fast-zip.core :as z]
            [lark.structure.coords :as coords]
            [lark.structure.loc :as loc]
            [lark.structure.path :as path]
            [spell-spec.alpha :as ss]
            [cljs.spec.alpha :as s]
            [lark.tree.nav :as nav]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.tree.reader :as rd]
            [chia.util :as u]))

(def debug? false)

(defn print! [& args]
  (when debug? (apply print args)))

;;;;;;;;;;;;;;;;;;;;;
;;
;; Resolve
;;
;; - for resolving pointers into absolute coordinates

(defn end-pos? [path]
  (when (= :end (path/get-last path))
    :end-inner))

(defn- resolve-node-offset [node [path [line+ col+]]]
  (if (= :end (path/get-last path))
    (:to (coords/inner-span node))
    (let [[line col] (:range node)]
      (coords/clamp [(+ line+ line)
                     (cond-> col+
                             (zero? line+) (+ col))] node))))

(defn resolve
  "Returns absolute coordinates for pointer."
  [root-loc [path offset :as pointer]]
  (let [loc (loc/get-loc root-loc path)
        node (some-> loc z/node)]
    (resolve-node-offset node pointer)))

(defn resolve-span
  "Returns absolute coordinates for span of pointers."
  [loc {:keys [from to]}]
  {:from (some->> from (resolve loc))
   :to (some->> to (resolve loc))})

;;;;;;;;;;;;;;;;;;;;;
;;
;; Pointers
;;
;; - turning absolute coordinates into a path-based pointer

(do

  (declare pointer)

  (defn normalize
    [[path offset :as the-pointer] loc coords]
    (print! "   norm:" the-pointer coords)
    (let [node (z/node loc)
          leftl (z/left loc)
          rightl (z/right loc)
          upl (z/up loc)
          leftn (some-> leftl z/node)
          rightn (some-> rightl z/node)
          terminal? (n/terminal? node)
          coll? (n/may-contain-children? node)
          children (seq (:children node))
          {inner-from :from
           inner-to :to} (coords/inner-span node)
          outer-left? (coords/= coords (:from node))
          outer-right? (coords/= coords (:to node))
          inner-left? (coords/= coords inner-from)
          inner-right? (coords/= coords inner-to)
          terminal (or (if (and outer-left?
                                leftn
                                (n/terminal? leftn))
                         (pointer leftl coords)
                         (when (n/terminal? node)
                           the-pointer)))
          normalized (cond terminal terminal
                           inner-right? (if (some-> children last n/terminal?)
                                          (pointer (last (nav/child-locs loc)) coords)
                                          the-pointer)
                           ;; never stay at outer-right
                           outer-right? (if rightl
                                          (pointer rightl coords)
                                          (pointer upl coords))
                           :else the-pointer)]
      (print! "   norz:" normalized)
      normalized))

  (defn pointer
    "Returns a pointer (path, offset) for an position within a loc.

    position:    {... :line, :column}
    cursor-path: [loc-path offset]
    offset:      [line column]"
    [loc coords]
    (let [node (.-node loc)
          tag (.-tag node)
          coords (coords/clamp coords node)
          inner-span (coords/inner-span node)
          terminal? (n/terminal? node)

          in-left (coords/< coords (:from inner-span))
          right-edge? (and (coords/= coords (:to node))
                           (second (rd/edges tag))
                           (not (z/right loc)))]
      (print! "\nPointer" (:tag node) (select-keys inner-span [:from :to]))
      (print! "   find:" coords)

      (when terminal? (print! "  " :terminal))
      (when in-left (print! "  " :in-left))
      (when right-edge? (print! "  " :right-edge))

      (cond (or terminal?
                in-left)

            (do (print! "  " 1 (:from node) coords (:range node) (prn-str (emit/string node)))
                (normalize [(loc/path loc)
                            (coords/offset (:from node) coords)] loc coords))

            (= coords (:to inner-span)) (do (print! "  " 2 :end)
                                            (normalize [(conj (loc/path loc) :end)
                                                        [0 0]] loc coords))

            :else
            (let [child (->> (nav/child-locs loc)
                             (take-while (comp #(coords/<= (:from %) coords)
                                               z/node))
                             (last))]
              (print! "  " :child child)
              (assert child "must find child")
              (recur child coords)))))

  #_(let [s "[[ab]] [][] [cd] "
          coords [0 2]
          z (tree/string-zip s)
          p (pointer z coords)]
      p)
  #_(let [s "[()]"]
    [s
     (u/for-map [coords [[0 0]
                         [0 1]
                         [0 2]
                         [0 3]
                         [0 4]
                         [0 5]]]                            ;; should still be the space?
       (let [z (tree/string-zip s)
             pt (pointer z coords)
             node (some->> (first pt)
                           (loc/get-loc z)
                           z/node)]
         (assert (= (resolve-node-offset node pt)
                    (coords/clamp coords (z/node z))))
         ;; ISSUE...
         ;; ok, so we represent the position in an empty list, or at the end of a non-terminal in last position of a collection,
         ;; as the "index beyond the last child." Well, how do we _work_ with that?
         ;; we have all these functions that expect to get a `loc`. do we _invent_ one?
         ;; (|)  (()|)
         ;; how do we _insert_ there? a non-printable marker loc? :marker, sexp nil, print nil, join-always. it has a position, but no value.
         ;;

         {`(~(serialize/write-pointer-spans s [{:from coords}])
            ~@coords)
          `(~@pt ~(emit/string node))
          }))]))

(defn same-paths? [span]
  (coords/point?
   (u/update-some-keys span [:from :to] first)))

(defn resolve-coord-span [loc span]
  (u/update-some-keys span [:from :to] #(pointer loc %)))

(s/fdef resolve-coord-span
        :args (s/cat :loc ::loc/loc
                     :span ::coords/span))

(s/def ::pointer (s/tuple ::path/path
                          ::coords/coord))

(s/def ::from ::pointer)
(s/def ::to ::pointer)

(s/def ::span
  (ss/strict-keys
   :opt-un [::from
            ::to]))

(s/fdef resolve
        :args (s/cat :loc ::loc/loc
                     :pointer ::pointer)
        :ret ::coords/coord)

#_(defn pointer---
    "Returns a pointer (path, offset) for a position within a loc.

    position:    {... :line, :column}
    cursor-path: [loc-path offset]
    offset:      [line column]"
    [loc coords]

    (let [node (.-node loc)
          tag (.-tag node)
          coords (coords/clamp coords node)
          inner-span (coords/inner-span node)
          terminal? (n/terminal? node)

          in-left (coords/< coords (:from inner-span))
          on-left (coords/= coords (:from node))
          in-right (coords/within-right-edge? node coords)
          right-edge? (and (coords/= coords (:to node))
                           (second (rd/edges tag))
                           (not (z/right loc)))
          no-children (empty? (:children node))]
      (print! "\n\n"
              (:tag node) [(:line node) (:column node)] (emit/string node) coords (mapv :tag (:children node)))
      (when terminal? (print! "  " :terminal))
      (when in-left (print! "  " :in-left))
      (when right-edge? (print! "  " :right-edge))

      (cond (or terminal?
                in-left
                (and no-children
                     (or (not right-edge?)
                         on-left)))

            (do (print! 1 (:from node) coords (:range node) (prn-str (emit/string node)))
                [(loc/path loc)
                 (coords/offset (:from node) coords)])

            ;; different 'right' priority.
            ;; only shortcut to `right` here if we are on the outer edge + no z/right?
            (or right-edge?
                no-children) (do (print! 2 :end)            ;; WRONG
                                 [(loc/path loc)
                                  (sticky-end node inner-span coords)])

            :else
            (let [child (->> (nav/child-locs loc)
                             (take-while (comp #(coords/<= (:from %) coords)
                                               z/node))
                             (last))]
              (print! :child child)
              (assert child "must find child")
              (recur child coords)))))
