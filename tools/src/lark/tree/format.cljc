(ns lark.tree.format
  (:require [clojure.string :as str]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.nav :as nav]
            [fast-zip.core :as z]
            [chia.util.perf :as perf]
            [cljs.pprint :as pp]))

(def ^:dynamic *pretty* false)
(def ^:dynamic *cursor* nil)

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

(def FUNCTION_INDENT 0)                                     ;; could also be :indent

(defn operator-indentation [grandparent-op parent-op]
  (cond (nil? parent-op) :none
        (and grandparent-op
             (identical? grandparent-op "ns")
             (keyword? parent-op)) 0
        :else
        (case parent-op
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
          (cond (str/starts-with? parent-op "def") :indent
                (re-find #"with|when|if" parent-op) 1
                ;(str/ends-with? x "->") 1
                :else FUNCTION_INDENT))))

(defn butlast-vec [v]
  (cond-> v
          (not (empty? v)) (pop)))

(defn body-indent
  [{:keys [parent
           parent-op
           grandparent-op]
    :as opts} siblings]
  (let [column (:column parent)
        tag (.-tag parent)
        inner-column (+ column
                        (some-> (rd/edges tag)
                                (first)
                                (.-length)))]
    (if-not (perf/unchecked-keyword-identical? :list tag)
      inner-column
      (let [operator (first siblings)]
        (let [indent-type (operator-indentation grandparent-op parent-op)]
          (case indent-type
            :none inner-column
            :indent (inc inner-column)
            (let [threading-form (some-> grandparent-op (str/ends-with? "->"))
                  indent-offset (-> indent-type
                                    (cond->
                                     threading-form (dec)))
                  split-after (+ 2 indent-offset)
                  [exact? taken _ num-passed] (->> (cond-> siblings
                                                           (n/whitespace? operator) (butlast-vec))
                                                   (rd/split-after-n split-after
                                                                     n/sexp?
                                                                     (fn [node]
                                                                       (perf/unchecked-keyword-identical? :newline (.-tag node)))))]
              (cond exact? (:column (last taken))
                    (and (identical? num-passed 1)
                         (not threading-form)) inner-column
                    :else (inc inner-column)))))))))

(defn whitespace-tag? [t]
  (perf/keyword-in? [:space :cursor :selection :tab :newline]
                    t))

(defn pad-chars?
  "Returns true if space should be left inbetween characters c1 and c2."
  [c1 c2]
  (if (or (rd/close-bracket? c2)
          (rd/open-bracket? c1)
          (rd/prefix-boundary? c1)
          (identical? \# c1))
    false
    true))