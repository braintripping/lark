(ns lark.structure.delta
  (:refer-clojure :exclude [apply resolve])
  (:require [chia.util :as u]
            [lark.tree.emit :as emit]                       ;; for macro

            [lark.structure.coords :as coords]
            [cljs.spec.alpha :as s]
            [cljs.pprint :as pp]
            [lark.structure.path :as path])
  (:require-macros [lark.structure.delta]))

(def ^:dynamic *deltas* nil)

(defn tracked* [pointer-span]
  (let [a (atom nil :validator #(s/assert :lark.structure.pointer/span %))]
    (reset! a pointer-span)
    a))

(defn tracked [pointer-span]
  (assert *deltas* "tracked spans only work inside a transaction")
  (let [span (tracked* pointer-span)]
    (vswap! *deltas* update :tracked conj span)
    span))

(defn update-span-pointers [f span]
  (u/update-some-keys span [:from :to] (fn [span] (f span))))

(defn update-pointers! [pointerf]
  (let [{:as deltas
         :keys [selections
                tracked]} @*deltas*
        update-span #(update-span-pointers pointerf %)]
    (doseq [span selections]
      (swap! span update-span))
    (doseq [span tracked]
      (swap! span update-span))))

(defn update-paths! [pathf]
  (update-pointers! (fn [[path offset]]
                      [(pathf path) offset])))

(defn update-spans! [spanf]
  (let [{:as deltas
         :keys [selections
                tracked]} @*deltas*]
    (doseq [span selections]
      (swap! span spanf))
    (doseq [span tracked]
      (swap! span spanf))))

(defn add-offset!
  [[path from-coords] offset]
  #_(prn :add-offset! path from-coords offset)
  (update-pointers! (fn [pointer]
                      (let [[p coords :as pointer] pointer]
                        (if-let [after (when (and (= p path)
                                                  (coords/<= from-coords coords))
                                         [p (coords/+ coords offset)])]
                          (do
                            #_(print "  " [p coords])
                            #_(print "  " after "\n\n")
                            after)
                          pointer)))))

(s/fdef add-offset!
        :args (s/cat :pointer :lark.structure.pointer/pointer
                     :offset ::coords/coord))

(defn selections []
  (assert *deltas* "`selections` only work inside a transaction")
  (:selections @*deltas*))

(defn print-selections []
  #_#_(prn :selected)
      (doseq [s (selections)]
        (print "  " @s)))

(defn pointer-offset
  [offsets [path start-coords]]
  (reduce coords/+ start-coords (->> (subseq offsets
                                             >= (concat path [0 0])
                                             < (concat path start-coords))
                                     (vals))))

(defn update-span-paths [f span]
  (u/update-some-keys span [:from :to]
                      (fn [[path offset]] [(f path) offset])))

(defn shift-sibling [depth i added path]
  (let [prior-sibling? (and (>= (count path) depth)
                            (>= (nth path (dec depth)) i))]
    (if prior-sibling?
      (path/update-nth path (dec depth) #(max 0 (+ % added)))
      path)))

(defn shift! [path added]
  (let [depth (count path)
        i (peek path)]
    (update-paths!
     (fn [p]
       (shift-sibling depth i added p)))))

(defn move-offsets! [from-path to-path start-offset]
  (update-pointers! (fn [[path offset :as pointer]]
                      (if (= path from-path)
                        [to-path (coords/+ offset start-offset)]
                        pointer))))

(defn move-path [from-path to-path after-child path]
  (if (path/starts-with? path from-path)
    (let [moved (if (= (count path)
                       (count from-path))
                  to-path
                  (-> (into to-path (subvec path (count from-path)))
                      (path/update-last #(+ % after-child))))]
      (prn :MOVED path moved)
      moved)
    path))

(defn move-coll-path! [from to after]
  (update-paths! #(move-path from to after %)))

(s/def ::shift number?)

(s/def ::shifts (s/and
                 #(satisfies? ISorted %)
                 (s/map-of :lark.structure.path/path ::shift)))
(s/fdef shift!
        :args (s/cat :path :lark.structure.path/path
                     :added number?))

(comment
 (= (shift-sibling '(1 0) 1 '(1 0 1)) '(1 1 1)))