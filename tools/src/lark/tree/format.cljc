(ns lark.tree.format
  (:require [clojure.string :as str]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.util :as util]
            [lark.tree.range :as range]
            [fast-zip.core :as z]))

(def INDENT " ")

(defn repeat-string [content n]
  (loop [i 0
         out ""]
    (if (= i n)
      out
      (recur (inc i) (str out content)))))

(def ^:dynamic *pretty* false)

(defn emit-space? [loc]
  (and (some? (z/left loc))
       (some? (z/right loc))
       (not (n/newline? (some-> (z/left loc)
                                (z/node))))))

(defn indentation-for [x]
  (case x

    ("bound-fn" "extend" "extend-protocol" "extend-type" "fn" "ns" "reify")
    :indent

    ("cond" "do" "finally" "try" "with-out-str" "go")
    0

    ("assoc" "apply" "binding" "case" "definterface" "defstruct" "deftype" "doseq" "dotimes" "doto"
     "for" "if" "if-let" "if-not" "if-some" "let" "letfn" "locking" "loop"
     "struct-map" "when" "when-first" "when-let" "when-not" "when-some"
     "while" "with-bindings" "with-local-vars" "with-open" "with-redefs"
     "with-redefs-fn" "go-loop" "are" "deftest" "testing")
    1

    ("catch" "condp" "proxy")
    2
    (cond (str/starts-with? x "def") :indent
          (re-find #"with|when|if" x) 1
          ;(str/ends-with? x "->") 1
          :else 0)))
(defn threading-node?
  [node]
  (when-let [operator (and (= (get node :tag) :list)
                           (some-> node
                                   (get :children)
                                   (first)))]
    (and (= :symbol (get operator :tag))
         (str/ends-with? (name (get operator :value)) "->"))))

(defn node-length [{:keys [column end-column tag]}]
  (case tag :space 1
            :tab 1
            (:cursor :selection) 0
            (- end-column column)))

(defn whitespace-tag? [t]
  (util/contains-identical-keyword? [:space :cursor :selection :tab :newline]
                                    t))

(defn body-indent*
  ([indent-level node] (body-indent* indent-level node nil))
  ([indent-level loc child]
   (assert (number? indent-level))
   (let [{:keys [children tag]} (z/node loc)
         {:as operator
          op-tag :tag
          op-value :value} (first children)
         threading? (and (= tag :list)
                         (some-> loc
                                 (z/up)
                                 (z/node)
                                 (threading-node?)))]
     (if (and (= :list tag)
              (= :symbol op-tag))
       (let [indent-type (indentation-for (name op-value))]
         (case indent-type
           :indent (+ indent-level 1)
           (let [indent-offset (-> indent-type
                                   (cond-> threading? (dec)))
                 [exact? taken remaining num-passed] (->> (cond->> children
                                                                   (n/whitespace? (first children)) (drop 1))
                                                          (rd/split-after-n (+ 2 indent-offset)
                                                                            (comp (complement whitespace-tag?) :tag)
                                                                            (fn [node]
                                                                              (or (= :newline (get node :tag))
                                                                                  (= node child)))))]
             (+ indent-level (cond exact? (reduce + 0 (mapv node-length (drop-last taken)))
                                   (= num-passed 1) 0
                                   :else 1)))))
       (+ indent-level)))))


(defn indentation-parent? [node]
  (util/contains-identical-keyword? [:vector :list :map] (get node :tag)))

(defn body-indent-string [pos child-loc]
  (if-let [coll-loc (->> (iterate z/up child-loc)
                         (sequence (comp (take-while identity)
                                         (filter #(range/within-inner? (z/node %) pos))
                                         (filter (comp indentation-parent? z/node))))
                         (first))]
    (let [coll-node (z/node coll-loc)]
      (let [child (z/node child-loc)
            left-edge-width (count (first (get rd/edges (get coll-node :tag))))
            body-indent (+ left-edge-width (body-indent* (:column coll-node) coll-loc child))]
        (repeat-string INDENT body-indent)))
    0))

(defn pad-chars?
  "Returns true if space should be left inbetween characters c1 and c2."
  [c1 c2]
  (cond (rd/close-bracket? c2) false
        (rd/open-bracket? c1) false
        :else true))