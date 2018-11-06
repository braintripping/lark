(ns lark.tree.format
  (:require [clojure.string :as str]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.nav :as nav]
            [fast-zip.core :as z]
            [chia.util.perf :as perf]
            [cljs.pprint :as pp]))

(def ^:dynamic *pretty* false)

(def SPACES (reduce #(perf/str %1 " ") "" (range 500)))

(defn spaces [n]
  (if (< n 500)
    (subs SPACES 0 n)
    (str/join (take n (repeat " ")))))

(defn count-lines [^string s]
  (loop [i -1
         found 0]
    (let [i (.indexOf s "\n" (inc i))]
      (if (identical? i -1)
        found
        (recur i (inc found))))))

(defn last-line-length [s]
  (let [last-line-start (.lastIndexOf s \newline)]
    (when-not (identical? -1 last-line-start)
      (- (.-length s) last-line-start))))

(defn emit-space? [loc]
  (and (some? (nav/right-node loc))
       (some->> (nav/left-node loc)
                .-tag
                (perf/identical? :newline)
                (not))))

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
  [indent-level loc]
  (assert (number? indent-level))
  (let [node (.-node loc)
        tag (.-tag node)]
    (if-not (perf/identical? :list tag)
      indent-level
      (let [children (.. loc -node -children)
            operator (first children)]
        (if-not (perf/identical? :token (some-> operator .-tag))
          indent-level
          (let [indent-type (indentation-for (name (.-value operator)))]
            (if (perf/identical? :indent indent-type)
              (+ indent-level 1)
              (let [threading? (and (perf/identical? :list tag)
                                    (some-> (nav/up-node loc)
                                            (threading-node?)))
                    indent-offset (-> indent-type
                                      (cond-> threading? (dec)))
                    split-after (+ 2 indent-offset)
                    [exact? taken _ num-passed] (->> (cond-> children
                                                             (n/whitespace? operator) (butlast-vec))
                                                     (rd/split-after-n split-after
                                                                       n/sexp?
                                                                       (fn [node]
                                                                         (perf/identical? :newline (.-tag node)))))]
                (+ indent-level (cond exact? (reduce + 0 (mapv node-length (pop taken)))
                                      (and (identical? num-passed 1)
                                           (not threading?)) 0
                                      :else 1))))))))))

(defn pad-chars?
  "Returns true if space should be left inbetween characters c1 and c2."
  [c1 c2]
  (if (or (rd/close-bracket? c2)
          (rd/open-bracket? c1)
          (rd/prefix-boundary? c1)
          (identical? \# c1))
    false
    true))