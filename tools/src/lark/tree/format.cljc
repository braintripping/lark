(ns lark.tree.format
  (:require [clojure.string :as str]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.util :as util]
            [lark.tree.range :as range]
            [fast-zip.core :as z]
            [chia.util.perf :as perf]))

(def SPACES (str/join (take 200 (repeat " "))))

(defn spaces [n]
  (subs SPACES 0 n))

(def ^:dynamic *pretty* false)

(defn emit-space? [loc]
  (and (when-let [left (z/left loc)]
         (not (n/newline? (z/node left))))
       (some? (z/right loc))))

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
  (when-let [operator (and (perf/identical? :list (.-tag node))
                           (some-> (.-children node)
                                   (first)))]
    (and (perf/identical? :token (.-tag operator))
         (str/ends-with? (.-value operator) "->"))))

(defn node-length [{:as node :keys [column end-column]}]
  (case (.-tag node) :space (if (identical? node rd/*active-cursor-node*)
                              (count (.-value node))
                              1)
                     :tab 1
                     (:cursor :selection) 0
                     (- end-column column)))

(defn whitespace-tag? [t]
  (perf/keyword-in? [:space :cursor :selection :tab :newline]
                    t))

(defn butlast-vec [v]
  (cond-> v
          (not (empty? v)) (pop)))

(defn body-indent*
  ([indent-level node] (body-indent* indent-level node nil))
  ([indent-level loc child]
   (assert (number? indent-level))
   (let [node (.-node loc)
         tag (.-tag node)]
     (if-not (perf/identical? :list tag)
       indent-level
       (let [children (.-children node)
             operator (first children)]
         (if-not (perf/identical? :token (some-> operator .-tag))
           indent-level
           (let [indent-type (indentation-for (name (.-value operator)))]
             (if (perf/identical? :indent indent-type)
               (+ indent-level 1)
               (let [threading? (and (perf/identical? :list tag)
                                     (some-> (z/up loc)
                                             (.-node)
                                             (threading-node?)))
                     indent-offset (-> indent-type
                                       (cond-> threading? (dec)))
                     split-after (+ 2 indent-offset)
                     [exact? taken _ num-passed] (->> (cond-> children
                                                              (n/whitespace? operator) (butlast-vec))
                                                      (rd/split-after-n split-after
                                                                        n/sexp?
                                                                        (fn [node]
                                                                          (or (perf/identical? :newline (.-tag node))
                                                                              (identical? node child)))))]
                 (+ indent-level (cond exact? (reduce + 0 (mapv node-length (pop taken)))
                                       (and (identical? num-passed 1)
                                            (not threading?)) 0
                                       :else 1)))))))))))


(defn indentation-parent? [node]
  (perf/keyword-in? [:vector :list :map] (.-tag node)))

(defn body-indent-string [pos child-loc]
  (if-let [coll-loc (->> (iterate z/up child-loc)
                         (sequence (comp (take-while identity)
                                         (filter #(range/within-inner? (z/node %) pos))
                                         (filter (comp indentation-parent? z/node))))
                         (first))]
    (let [coll-node (z/node coll-loc)]
      (let [child (z/node child-loc)
            left-edge-width (count (first (rd/edges (.-tag coll-node))))
            body-indent (+ left-edge-width (body-indent* (:column coll-node) coll-loc child))]
        (spaces body-indent)))
    0))

(defn pad-chars?
  "Returns true if space should be left inbetween characters c1 and c2."
  [c1 c2]
  (if (or (rd/close-bracket? c2)
          (rd/open-bracket? c1)
          (rd/prefix-boundary? c1)
          (identical? \# c1))
    false
    true))