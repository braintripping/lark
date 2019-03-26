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
            [lark.tree.core :as tree]
            [lark.tree.node :as n]
            [lark.tree.reader :as rd]
            [chia.util :as u]
            [lark.structure.serialize-selections :as serialize]
            [cljs.core :as core]))

(def debug? false)

(defn print! [& args]
  (when debug? (apply print args)))

(def sticky-constant 100)

;;;;;;;;;;;;;;;;;;;;;
;;
;; Resolve
;;
;; - for resolving pointers into absolute coordinates

(defn- resolve-sticky-end [node end-col col+]
  ;; handles cases where a pointer into a collection
  ;; addresses its right edge, indicated by the col-offset
  ;; being greater than the width of the left edge.
  (when (n/may-contain-children? node)
    (let [[L R] (rd/edges (:tag node))
          left-width (count L)
          inner-end-col (- end-col (count R))]
      (when (> col+ left-width)
        (assert (= (-> inner-end-col (+ col+) (- left-width sticky-constant)) inner-end-col) "resolve: end positions must be on inner-end")
        inner-end-col))))

(defn end-pos [loc [_ col+]]
  (let [{:as node
         :keys [tag]} (z/node loc)]
    (when (and (n/may-contain-children? node)
               (> col+ (count (first (rd/edges tag)))) )
      (assert (let [left-offset (- col+ (count (first (rd/edges tag))) sticky-constant)] (zero? left-offset)) "End positions must be on the inside of the loc") ;(= left-offset right-width) :end-outer
      :end-inner)))

(defn- resolve-node-offset [node [line+ col+]]
  ;; paths only tell us which node to look at.
  ;; then we must resolve the offset _into_ the node.
  (let [[line col end-line end-col] (:range node)
        coords (if-let [sticky-end (resolve-sticky-end node end-col col+)]
                 [end-line sticky-end]
                 [(+ line+ line) (cond-> col+
                                         (zero? line+) (+ col))])]
    (coords/clamp coords node)))

(defn resolve
  "Returns absolute coordinates for pointer."
  [root-loc [path offset :as pointer]]
  (let [loc (loc/get-loc root-loc path)
        node (some-> loc z/node)]
    (resolve-node-offset node offset)))

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

(defn sticky-end [node inner-span coords]
  (let [[line col] (coords/offset (:to inner-span) coords)
        left-width (- (second (:from inner-span))
                      (second (:from node)))]
    (print! :Sticky-End-For coords)
    [line (+ col sticky-constant left-width)]))

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
                           ;; never outer-right?
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
          on-left (coords/= coords (:from node))
          in-right (coords/within-right-edge? node coords)
          right-edge? (and (coords/= coords (:to node))
                           (second (rd/edges tag))
                           (not (z/right loc)))
          no-children (empty? (:children node))]
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
                                            (normalize [(loc/path loc)
                                                        (sticky-end node inner-span coords)] loc coords))

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
  #_(let [s "()"]
      [s
       (u/for-map [coords [[0 0]
                           [0 1]
                           [0 2]
                           [0 3]
                           [0 4]]]                          ;; should still be the space?
         (let [z (tree/string-zip s)
               pt (pointer z coords)
               node (some->> (first pt)
                             (loc/get-loc z)
                             z/node)]
           (assert (= (resolve-node-offset node (second pt))
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

(defn same-loc? [span]
  (coords/point? (u/update-some-keys span [:from :to] first)))

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
