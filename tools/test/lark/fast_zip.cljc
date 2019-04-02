;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;   functional hierarchical zipper, with navigation, editing and enumeration
;;   see Huet

(ns ^{:doc "Functional hierarchical zipper, with navigation, editing,
  and enumeration.  See Huet"
      :author "Rich Hickey, modified by Alexander K. Hudek, modified by Matthew Huebert"}
lark.fast-zip
  (:require #?@(:cljs [[applied-science.js-interop :as j]
                       [goog.array :as garray]])
            [clojure.core :as core])
  (:refer-clojure :exclude [replace remove next]))

(deftype ZipperOps [branch? children make-node])

(deftype ZipperPath [l r ppath pnodes changed?])

(deftype ZipperLocation [^ZipperOps ops node ^ZipperPath path _sentinel])

(def sentinel :sentinel)

(defn sentinel? [loc]
  (.-_sentinel loc))

(defn- assoc-sentinel [loc value]
  (ZipperLocation. (.-ops loc)
                   (.-node loc)
                   (.-path loc)
                   value))

(defn as-sentinel [loc] (assoc-sentinel loc true))
(defn drop-sentinel [loc] (assoc-sentinel loc false))

(defn zipper
  "Creates a new zipper structure.

  branch? is a fn that, given a node, returns true if can have
  children, even if it currently doesn't.

  children is a fn that, given a branch node, returns a seq of its
  children.

  make-node is a fn that, given an existing node and a seq of
  children, returns a new branch node with the supplied children.
  root is the root node."
  {:added "1.0"}
  [branch? children make-node root]
  (ZipperLocation. (ZipperOps. branch? children make-node) root nil nil))

(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  {:added "1.0"}
  [root]
  (zipper
   seq?
   identity
   (fn [node children] (with-meta children (meta node)))
   root))

(defn vector-zip
  "Returns a zipper for nested vectors, given a root vector"
  {:added "1.0"}
  [root]
  (zipper
   vector?
   seq
   (fn [node children] (with-meta (vec children) (meta node)))
   root))

(defn xml-zip
  "Returns a zipper for xml elements (as from xml/parse),
  given a root element"
  {:added "1.0"}
  [root]
  (zipper
   (complement string?)
   (comp seq :content)
   (fn [node children]
     (assoc node :content (and children (apply vector children))))
   root))

(defn node
  "Returns the node at loc"
  [^ZipperLocation loc]
  (.-node loc))

(defn branch?
  "Returns true if the node at loc is a branch"
  [^ZipperLocation loc]
  ((.-branch? ^ZipperOps (.-ops loc)) (.-node loc)))

(defn children
  "Returns a seq of the children of node at loc, which must be a branch"
  [^ZipperLocation loc]
  ((.-children ^ZipperOps (.-ops loc)) (.-node loc)))

(defn make-node
  "Returns a new branch node, given an existing node and new children.
  The loc is only used to supply the constructor."
  [^ZipperLocation loc node children]
  ((.-make-node ^ZipperOps (.-ops loc)) node children))

(defn node-path
  "Returns a seq of nodes leading to this loc"
  [^ZipperLocation loc]
  (if-let [^ZipperPath p (.-path loc)] (.-pnodes p)))

(defn lefts
  "Returns a seq of the left siblings of this loc"
  [^ZipperLocation loc]
  (if-let [^ZipperPath p (.-path loc)] (seq (reverse (.-l p)))))

(defn rights
  "Returns a seq of the right siblings of this loc"
  [^ZipperLocation loc]
  (if-let [^ZipperPath p (.-path loc)] (.-r p)))

(defn down
  "Returns the loc of the leftmost child of the node at this loc,
  or nil if no children"
  [^ZipperLocation loc]
  (when (branch? loc)
    (when-let [cs (children loc)]
      (let [node (.-node loc), ^ZipperPath path (.-path loc)]
        (ZipperLocation.
         (.-ops loc)
         (first cs)
         (ZipperPath.
          '()
          #?(:clj (.next ^clojure.lang.ISeq cs) :cljs (cljs.core/next cs))
          path
          (if path (conj (.-pnodes path) node) [node])
          nil)
         nil)))))

(defn up
  "Returns the loc of the parent of the node at this loc, or nil if at the top"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (when-let [pnodes (and path (.-pnodes path))]
      (let [pnode (peek pnodes)]
        (if (.-changed? path)
          (ZipperLocation.
           (.-ops loc)
           (make-node loc pnode (concat (reverse (.-l path)) (cons (.-node loc) (.-r path))))
           (if-let [^ZipperPath ppath (.-ppath path)]
             (ZipperPath. (.-l ppath) (.-r ppath) (.-ppath ppath) (.-pnodes ppath) true))
           nil)
          (ZipperLocation.
           (.-ops loc)
           pnode
           (.-ppath path)
           nil))))))

(defn root
  "zips all the way up and returns the root node, reflecting any changes."
  [^ZipperLocation loc]
  (if (#?(:clj identical? :cljs =) :end (.-path loc))
    (.-node loc)
    (let [p (up loc)]
      (if p
        (recur p)
        (.-node loc)))))

(defn right
  "Returns the loc of the right sibling of the node at this loc, or nil"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (when-let [r (and path (.-r path))]
      (ZipperLocation.
       (.-ops loc)
       (first r)
       (ZipperPath.
        (conj (.-l path) (.-node loc))
        #?(:clj (.next ^clojure.lang.ISeq r) :cljs (cljs.core/next r))
        (.-ppath path)
        (.-pnodes path)
        (.-changed? path))
       nil))))

(defn rightmost
  "Returns the loc of the rightmost sibling of the node at this loc, or self"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (if-let [r (and path (.-r path))]
      (ZipperLocation.
       (.-ops loc)
       (last r)
       (ZipperPath.
        (apply conj (.-l path) (.-node loc) (butlast r))
        nil
        (.-ppath path)
        (.-pnodes path)
        (.-changed? path))
       nil)
      loc)))

(defn left
  "Returns the loc of the left sibling of the node at this loc, or nil"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (when (and path (seq (.-l path)))
      (ZipperLocation.
       (.-ops loc)
       (peek (.-l path))
       (ZipperPath.
        (pop (.-l path))
        (cons (.-node loc) (.-r path))
        (.-ppath path)
        (.-pnodes path)
        (.-changed? path))
       nil))))

(defn leftmost
  "Returns the loc of the leftmost sibling of the node at this loc, or self"
  [^ZipperLocation loc]
  (let [^ZipperPath path (.-path loc)]
    (if (and path (seq (.-l path)))
      (ZipperLocation.
       (.-ops loc)
       (last (.-l path))
       (ZipperPath.
        '()
        (concat
         #?(:clj  (.next ^clojure.lang.ISeq (reverse (.-l path)))
            :cljs (cljs.core/next (reverse (.-l path))))
         [(.-node loc)] (.-r path))
        (.-ppath path)
        (.-pnodes path)
        (.-changed? path))
       nil)
      loc)))

(defn insert-left
  "Inserts the item as the left sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (if-let [^ZipperPath path (.-path loc)]
    (ZipperLocation.
     (.-ops loc)
     (.-node loc)
     (ZipperPath. (conj (.-l path) item) (.-r path) (.-ppath path) (.-pnodes path) true)
     (.-_sentinel loc))
    (throw (new #?(:clj Exception :cljs js/Error) "Insert at top"))))

(defn insert-right
  "Inserts the item as the right sibling of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (assert (not (sentinel? loc)) "Cannot insert to the right of sentinel")
  (if-let [^ZipperPath path (.-path loc)]
    (ZipperLocation.
     (.-ops loc)
     (.-node loc)
     (ZipperPath. (.-l path) (cons item (.-r path)) (.-ppath path) (.-pnodes path) true)
     nil)
    (throw (new #?(:clj Exception :cljs js/Error) "Insert at top"))))

(defn replace
  "Replaces the node at this loc, without moving"
  [^ZipperLocation loc node]
  (assert (not (sentinel? loc)) "Cannot replace sentinel loc")
  (ZipperLocation.
   (.-ops loc)
   node
   (if-let [^ZipperPath path (.-path loc)]
     (ZipperPath. (.-l path) (.-r path) (.-ppath path) (.-pnodes path) true))
   nil))

(defn insert-child
  "Inserts the item as the leftmost child of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (replace loc (make-node loc (.-node loc) (cons item (children loc)))))

(defn append-child
  "Inserts the item as the rightmost child of the node at this loc, without moving"
  [^ZipperLocation loc item]
  (replace loc (make-node loc (.-node loc) (concat (children loc) [item]))))

(defn next
  "Moves to the next loc in the hierarchy, depth-first. When reaching
  the end, returns a distinguished loc detectable via end?. If already
  at the end, stays there."
  [^ZipperLocation loc]
  (let [path (.-path loc)]
    (if (#?(:clj identical? :cljs =) :end path)
      loc
      (or
       (if (branch? loc) (down loc))
       (right loc)
       (loop [p loc]
         (if-let [u (up p)]
           (or (right u) (recur u))
           (ZipperLocation. (.-ops loc) (.-node p) :end nil)))))))

(defn prev
  "Moves to the previous loc in the hierarchy, depth-first. If already at the root, returns nil."
  [loc]
  (if-let [lloc (left loc)]
    (loop [loc lloc]
      (if-let [child (and (branch? loc) (down loc))]
        (recur (rightmost child))
        loc))
    (up loc)))

(defn end?
  "Returns true if loc represents the end of a depth-first walk"
  [^ZipperLocation loc]
  (#?(:clj identical? :cljs =) :end (.-path loc)))

(defn remove
  "Removes the node at loc, returning the loc that would have preceded it in a depth-first walk."
  [^ZipperLocation loc]
  (if-let [^ZipperPath path (.-path loc)]
    (if (pos? (count (.-l path)))
      (loop [loc (ZipperLocation.
                  (.-ops loc)
                  (peek (.-l path))
                  (ZipperPath. (pop (.-l path)) (.-r path) (.-ppath path) (.-pnodes path) true)
                  nil)]
        (if-let [child (and (branch? loc) (down loc))]
          (recur (rightmost child))
          loc))
      (ZipperLocation.
       (.-ops loc)
       (make-node loc (peek (.-pnodes path)) (.-r path))
       (if-let [^ZipperPath ppath (.-ppath path)]
         (if ppath (ZipperPath. (.-l ppath) (.-r ppath) (.-ppath ppath) (.-pnodes ppath) true)))
       nil))
    (throw (new #?(:clj Exception :cljs js/Error) "Remove at top"))))

(defn edit
  "Replaces the node at this loc with the value of (f node args)"
  [^ZipperLocation loc f & args]
  (replace loc (apply f (.-node loc) args)))

;;;;;;;;;;;;;;;;
;;
;; Added

(defn path
  "Returns path to `loc` from root"
  [loc]
  (let [at-sentinel? (sentinel? loc)]
    (loop [^ZipperPath zpath (.-path loc)
           ret #?(:cljs (if at-sentinel? #js[sentinel] #js[])
                  :clj  (if at-sentinel? [sentinel] []))]
      (let [leftcount (when zpath (count (.-l zpath)))]
        (if-let [ppath (and zpath ^ZipperPath (.-ppath zpath))]
          (recur ppath
                 (#?(:cljs j/push!
                     :clj  conj) ret leftcount))
          #?(:cljs (vec (.reverse (cond-> ret
                                          leftcount (j/push! leftcount))))
             :clj  (vec (reverse (cond-> ret
                                         leftcount (conj leftcount))))))))))

(defn lefts-count
  "Returns a seq of the left siblings of this loc"
  [^ZipperLocation loc]
  (when-let [^ZipperPath p (.-path loc)] (count (.-l p))))

(defn rights-count
  "Returns a seq of the left siblings of this loc"
  [^ZipperLocation loc]
  (when-let [^ZipperPath p (.-path loc)] (count (.-r p))))

(def leftmost? (comp zero? lefts-count))

(defn rightmost? [loc]
  (or (sentinel? loc)
      (zero? (rights-count loc))))

(defn only-child? [loc]
  (and (leftmost? loc)
       (rightmost? loc)))

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

(defn- drop-common
  "Returns path1 and path2 with common ancestors removed"
  [path1 path2]
  (let [c1 (count path1)
        c2 (count path2)]
    (loop [i 0]
      (if (and (> c1 i)
               (> c2 i)
               (identical? (nth path1 i)
                           (nth path2 i)))
        (recur (inc i))
        [(subvec path1 i)
         (subvec path2 i)]))))

(defn- ^number compare-index
  "Comparator for segments, handles sentinel values."
  [x y]
  (cond
    (identical? x y) 0

    (nil? x) -1

    (nil? y) 1

    (number? x) (if (number? y)
                  (#?(:cljs garray/defaultCompare
                      :clj  core/compare) x y)
                  (if (keyword-identical? y sentinel)
                    -1
                    (throw (js/Error. (str "Cannot compare " x " to " y)))))

    (keyword-identical? x sentinel) (if (keyword-identical? y sentinel) 0 1)
    :else
    (throw (js/Error. (str "Cannot compare " x " to " y)))))

(defn sentinel-path? [p]
  (and (> (zero? (count p)))
       (= (peek p) sentinel)))

(defn nav
  "Moves `loc` to `to-path`"
  [loc to-path]
  (let [nav-to-sentinel? (sentinel-path? to-path)
        to-path (cond-> to-path
                        nav-to-sentinel? (pop))
        loc (cond-> loc
                    (sentinel? loc) (drop-sentinel))
        _ (prn [(path loc) to-path] :> (drop-common (path loc) to-path))
        [from to] (drop-common (path loc) to-path)
        loc (call-n up (dec (count from)) loc)
        from (seq (take 1 from))]
    (prn :from from :to to :P (path loc) :P2 (path (as-sentinel loc)))
    (loop [from from
           to to
           loc loc]
      (cond from
            (if (seq to)
              (case (compare-index (first from)
                                   (first to))
                -1 (recur (core/next from)
                          (core/next to)
                          (call-n right (- (first to)
                                           (first from)) loc))
                1 (recur (core/next from)
                         (core/next to)
                         (call-n left (- (first from)
                                         (first to)) loc)))
              (recur (core/next from)
                     to
                     (up loc)))
            (seq to)
            (recur from
                   (drop 1 to)
                   (->> (down loc)
                        (call-n right (first to))))
            :else (cond-> loc
                          nav-to-sentinel? (as-sentinel))))))

(defn to-child [loc i]
  (call-n right i (down loc)))

(defn top [loc]
  (loop [loc loc]
    (if-let [up (up loc)]
      (recur up)
      loc)))