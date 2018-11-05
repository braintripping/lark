(ns lark.tree.format
  (:require [clojure.string :as str]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.nav :as nav]
            [fast-zip.core :as z]
            [chia.util.perf :as perf]
            [lark.tree.emit :as emit]
            [cljs.pprint :as pp]))

(def ^:dynamic *pretty* false)

(def SPACES (reduce #(perf/str %1 " ") "" (range 500)))

(defn spaces [n]
  (if (< n 500)
    (subs SPACES 0 n)
    (str/join (take n (repeat " ")))))

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

(defn get-body-indent*
  [node threading?]
  (let [tag (.-tag node)
        inner-indent (+ (:column node)
                        (some-> (rd/edges tag)
                                (first)
                                (.-length)))]
    (if-not (perf/identical? :list tag)
      inner-indent
      (let [children (vec (.-children node))
            operator (first children)]
        (if-not (perf/identical? :token (some-> operator .-tag))
          inner-indent
          (let [indent-type (indentation-for (name (.-value operator)))]
            (if (perf/identical? :indent indent-type)
              (+ inner-indent 1)
              (let [
                    indent-offset (-> indent-type
                                      (cond-> threading? (dec)))
                    split-after (+ 2 indent-offset)
                    [exact? taken _ num-passed] (->> (cond-> children
                                                             (n/whitespace? operator) (butlast-vec))
                                                     (rd/split-after-n split-after
                                                                       n/sexp?
                                                                       (fn [node]
                                                                         (perf/identical? :newline (.-tag node)))))]
                (cond exact? (:column (last taken))
                      (and (identical? num-passed 1)
                           (not threading?)) inner-indent
                      :else (inc inner-indent))))))))))

(defn get-body-indent [loc]
  (when loc
    (get-body-indent* (.-node loc)
                      (and (perf/identical? :list (.-tag (.-node loc)))
                           (some-> (nav/up-node loc)
                                   (threading-node?))))))

(defn update-newline-loc [loc node current-indent]
  (if (identical? rd/*active-cursor-node* node)
    [loc
     (-> node .-value .-length)
     current-indent]
    (let [indent (if (nav/up-node loc)
                   (get-body-indent (z/up loc))
                   current-indent)
          changed? (not (identical? (-> node .-value .-length (dec))
                                    indent))
          indented-loc (cond-> loc
                               changed?
                               (z/edit assoc :value (str \newline
                                                         (spaces indent))))
          column-delta (- indent (dec (.-length (.-value node))))]
      [indented-loc
       column-delta
       indent])))

(defn apply-width-delta [node delta]
  #_(prn :apply-delta delta node)
  #_(cond-> node
            (identical? line (rd/get-line node))
            (update node :range #(update % 3 (fn [width] (+ width delta)))))
  (update node :range #(update % 3 (fn [width] (+ width delta)))))

(defn update-space-loc [loc node column-delta inner-delta indent]
  (cond (identical? rd/*active-cursor-node* node)
        [loc column-delta indent]

        (emit-space? loc)
        [(if (identical? (.-value node) " ")
           loc
           (z/edit loc assoc :value " "))
         (- column-delta (dec (.-length (.-value node))))
         indent]

        :else
        (let [space-length (.-length (.-value node))]
          [(cond-> (z/remove loc)
                   (nil? (nav/left-node loc)) (z/edit apply-width-delta (- inner-delta space-length)))
           (- column-delta space-length)
           indent])))

(defn next-with-column-delta [loc column-delta]
  [^z/ZipperLocation loc]
  (let [path (.-path loc)]
    (if (#?(:clj identical? :cljs =) :end path)
      loc
      (or
       (if (z/branch? loc) (z/down loc))
       (z/right loc)
       (let [line (rd/get-line (.-node loc))]
         (loop [p loc]
           (if-let [u (z/up p)]
             (or (z/right u) (recur (cond-> u
                                            (and (not (zero? column-delta))
                                                 (= line (:end-line (.-node u))))
                                            (z/edit apply-width-delta column-delta))))
             (new z/ZipperLocation (.-ops loc) (.-node p) :end))))))))

(defn format-zip [root-loc]
  (when root-loc
    (loop [loc (z/next root-loc)
           column-delta 0
           inner-delta 0
           indent 0
           i 0]
      (if (z/end? loc)
        (z/edit loc apply-width-delta column-delta)
        (let [node (.-node loc)
              tag (.-tag node)
              loc (cond-> loc
                          (not (zero? column-delta))
                          (z/edit update :column #(+ % column-delta)))
              [next-loc next-column-delta next-indent]
              (case tag
                :newline (update-newline-loc loc node indent)
                (:space
                 :tab) (update-space-loc loc node column-delta inner-delta indent)
                [loc column-delta indent])
              changes-here (- next-column-delta
                              column-delta)
              next-inner-delta (if (nav/right-node next-loc)
                                 (+ inner-delta changes-here)
                                 0)]

          (recur (next-with-column-delta next-loc (+ inner-delta changes-here))
                 next-column-delta
                 next-inner-delta
                 next-indent
                 (inc i)))))))