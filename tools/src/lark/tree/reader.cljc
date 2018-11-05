(ns lark.tree.reader
  (:refer-clojure :exclude [peek next])
  (:require
   [lark.tree.util :as util]
   [chia.util.perf :as perf]
   #?@(:cljs [[cljs.tools.reader.reader-types :as r]
              [chia.util.js-interop :as j]]
       :clj  [[clojure.tools.reader.reader-types :as r]])))

(def ^:dynamic *invalid-nodes* nil)
(def ^:dynamic *active-cursor-node* nil)

(def ^:dynamic *delimiter* (list))

(def peek r/peek-char)

(defn current-offset [indexing-pushback-reader]
  (let [pushback-reader (.-rdr indexing-pushback-reader)
        indexing-reader (.-rdr pushback-reader)
        pushback (- (.-buf_len pushback-reader)
                    (.-buf_pos pushback-reader))]
    (- (.-s_pos indexing-reader)
       pushback)))

(defn edges [tag]
  (case tag
    :comment [";"]
    :deref ["@"]
    :list [\( \)]
    :fn ["#"]
    :map [\{ \}]
    :meta ["^"]
    :quote ["'"]
    :reader-meta ["#^"]
    :raw-meta ["^"]
    :reader-macro ["#"]
    :regex ["#"]
    :set ["#"]
    :string [\" \"]
    :syntax-quote ["`"]
    :unquote ["~"]
    :unquote-splicing ["~@"]
    :uneval ["#_"]
    :var ["#'"]
    :vector [\[ \]]
    :reader-conditional ["#?"]
    :reader-conditional-splice ["#?@"]
    :selection [\‹ \›]
    nil))

(defn whitespace-tag? [tag]
  (perf/keyword-in?
   [:space :newline :tab :comma :cursor :selection]
   tag))

(defn close-bracket? [ch]
  (perf/identical-in? [\) \] \}] ch))

(defn open-bracket? [ch]
  (perf/identical-in? [\( \[ \{] ch))

(defn throw-reader
  "Throw reader exception, including line/column."
  [reader fmt & data]
  (let [c (.-column reader)
        l (.-line reader)]
    (throw
     (#?(:cljs js/Error.
         :clj  Exception.)
      (str fmt data
           " [at line " l ", column " c "]")))))

(defn read-while
  "Read while the chars fulfill the given condition. Does not consume the unmatching char."
  [reader pred]
  (loop [out ""]
    (let [c (r/read-char reader)
          passes? (pred c)]
      (cond (nil? c) (if passes?
                       (throw-reader reader "Unexpected EOF.")
                       out)
            passes? (recur (perf/str out c))
            :else (do
                    (r/unread reader c)
                    out)))))

(defn read-until
  "Read until a char fulfills the given condition. Does not consume the matching char."
  [reader p?]
  (read-while reader (complement p?)))

(defn next
  "Read next char."
  [reader]
  (r/read-char reader))

(defn ignore
  "Ignore the next character."
  [reader]
  (r/read-char reader))

(defn unread
  "Unreads a char. Puts the char back on the reader."
  [reader ch]
  (r/unread reader ch))

(defn position
  "Returns 0-indexed vector of [line, column] for current reader position."
  [reader]
  [(dec (.-line reader))
   (dec (.-column reader))])

;; TODO
;; :value => (first children)
;; first, last, rest, etc. -- operate on children
;; seq -- returns children

(defprotocol IRange
  ;; mutates range of node -- for internal parser use
  (assoc-range! [this position]))

(defprotocol IAppend
  (append [this x]))

(deftype Node [^:mutable tag
               ^:mutable options
               ^:mutable range
               ^:mutable value
               ^:mutable children]

  ;; ------------- Add child nodes via `append` --------------

  IAppend
  (append [coll o]
    (Node. tag options range value (conj children o)))

  ;; ------------- Position information stored via `meta` --------------

  IRange
  (assoc-range! [this position]
    (set! range position)
    this)

  ;; ------------- Equality --------------

  IEquiv
  (-equiv [o other]
   ;; position not taken into account
    (and (some? other)
         (keyword-identical? tag (.-tag other))
         (= value (.-value other))
         (= range (.-range other))
         (= children (.-children other))
         (= options (.-options other))))

  ;; ------------- Comparison by range --------------

  IComparable
  (-compare [x y]
    (let [l (- (-lookup x :line) (-lookup y :line))]
      (if (not= l 0)
        l
        (- (-lookup x :column) (-lookup y :column)))))

  ;; ------------- Associative operations on `options` --------------

  IAssociative
  (-contains-key? [this key]
    (case key :tag true
              (:line
               :column
               :end-line
               :end-column) (some? meta)
              :value (some? value)
              :children (some? children)
              :range (some? range)
              (contains? options key)))
  (-assoc [this k VAL]
    (case k
      :tag (Node. VAL options range value children)
      :options (Node. tag VAL range value children)
      :value (Node. tag options range VAL children)
      :children (Node. tag options range value VAL)
      :range (Node. tag options VAL value children)
      :line (Node. tag options (assoc range 0 VAL) value children)
      :column (Node. tag options (assoc range 1 VAL) value children)
      (Node. tag (assoc options k VAL) range value children)))

  ITransientAssociative
  (-assoc! [this k val]
    (case k
      :tag (set! tag val)
      :value (set! value val)
      :children (set! children val)
      :range (set! range val)
      :options (set! options val))
    this)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  ;; `get` supports direct access to tag, value, and positional elements

  ILookup
  (-lookup [this key]
    (case key :tag tag
              :value value
              :children children
              :range range
              :line (nth range 0)
              :column (nth range 1)
              :end-line (+ (nth range 0) (nth range 2))
              :end-column (+ (nth range 1) (nth range 3))
              :offset (nth range 4)
              :end-offset (nth range 5)
              :options options

              ;; todo
              ;; see if we should keep this
              :start {:line (nth range 0)
                      :column (nth range 1)}
              :end {:line (+ (nth range 0) (nth range 2))
                    :column (+ (nth range 1) (nth range 3))}
              (get options key nil)))
  (-lookup [this key not-found]
    (or (-lookup this key) not-found))

  ;; for debugging
  IPrintWithWriter
  (-pr-writer [o writer _]
    (let [options (dissoc options :string :invalid-nodes :cursor)]
      (-write writer (str (cond-> [tag]
                                  range (conj (subvec range 0 4))
                                  (seq options) (conj options)
                                  (string? value) (conj (subs value 0 10))
                                  children (conj children)) ">")))))

(defn delimiter-error [tag reader]
  (let [[line col] (position reader)]
    (Node. :error {:tag tag
                   :expected (first *delimiter*)} [line
                                                   col
                                                   line
                                                   (inc col)] nil nil)))

(defn current-pos [reader]
  [(dec (.-line reader))
   (dec (.-column reader))])

(defn read-with-position
  "Use the given function to read value, then attach row/col metadata."
  [reader read-fn]
  (let [start-line (dec (.-line reader))
        start-column (dec (.-column reader))
        start-offset (current-offset reader)]
    (when-let [node (read-fn reader)]
      (assoc-range!
       node
       [start-line
        start-column

        (- (dec (.-line reader))
           start-line)
        (- (dec (.-column reader))
           start-column)

        start-offset
        (current-offset reader)]))))

(defn report-invalid! [node]
  (let [node (assoc node :invalid? true)]
    (some-> *invalid-nodes*
            (vswap! conj node))
    node))

(defn InvalidToken!
  ([tag value] (InvalidToken! tag value nil))
  ([tag value position]
   (report-invalid!
    (->Node :token {:invalid? true
                    :info {:tag tag}} position value nil))))

(defn Splice
  ([children]
   (->Node :splice nil nil nil children))
  ([node children]
   (Splice (into [node] children))))

(defn CollectionNode [tag nodes]
  (->Node tag nil nil nil nodes))

(defn ValueNode [tag value]
  (->Node tag nil nil value nil))

(defn EmptyNode [tag]
  (->Node tag nil nil nil nil))

(defn split-after-n
  "Splits after `n` values which pass `pred`.

  Returns vector of the form
  [<took-n-values?> <taken-values> <remaining-values>]"
  [n pred stop? coll]
  (let [end (count coll)]
    (loop [i 0
           counted 0
           taken []]
      (cond (identical? counted n) [true taken (subvec coll i) counted]
            (identical? i end) [false taken [] counted]
            :else
            (let [next-item (nth coll i)]
              (if (and (some? stop?)
                       (stop? next-item))
                [false taken (subvec coll i) counted]
                (recur (inc i)
                       (cond-> counted
                               (pred next-item) (inc))
                       (conj taken next-item))))))))

(defn take-children
  [reader {:keys [:read-fn
                  :count-pred]
           take-n :take-n}]
  ;; returns `child-values, remaining-values, valid?`
  (loop [i 0
         out []]
    (if (> i 10000)
      (do
        (prn :take-children out)
        (js/console.error (js/Error. "Infinite loop?"))
        [false out nil])
      (if (and (some? take-n)
               (identical? i take-n))
        [true out nil]
        (let [next-node (read-fn reader)]
          (if (nil? next-node)
            [false out nil]
            (let [next-i (if (and (some? take-n) (some? count-pred))
                           (cond-> i
                                   (count-pred next-node) (inc))
                           (inc i))
                  tag (.-tag next-node)
                  value (.-value next-node)
                  children (.-children next-node)]
              (case tag
                :unmatched-delimiter
                (if
                 (contains? (set *delimiter*) value)        ;; can match prev
                  (do
                    (unread reader value)
                    [false out nil])
                  (recur next-i (conj out (report-invalid! next-node))))

                :splice
                (if take-n
                  (split-after-n take-n count-pred nil children)
                  (recur next-i (into out children)))

                :eof
                [false out nil]

                :matched-delimiter
                (if (and take-n (not (identical? take-n i)))
                  (do (unread reader value)
                      [false out nil])
                  [true out nil])

                (recur next-i (conj out next-node))))))))))

(defn- invalid-exit [coll-node reader out]
  (let [coll-tag (.-tag coll-node)
        [inner-line inner-col] (current-pos reader)
        inner-offset (current-offset reader)]
    (case coll-tag
      :base (assoc coll-node :children out)
      (Splice (let [[left right] (edges coll-tag)
                    width (count left)]
                (report-invalid!
                 (doto (EmptyNode :unmatched-delimiter)
                   (-> .-options
                       (set! {:info {:tag coll-tag
                                     :direction :forward
                                     :expects right}}))
                   (-> .-range
                       (set! [inner-line
                              (- inner-col width)
                              0 #_inner-line
                              width #_inner-col
                              (- inner-offset width)
                              inner-offset]))
                   (-> .-value
                       (set! left))))) out))))

(defn- valid-exit [coll-node out]
  (set! (.-children coll-node) out)
  coll-node)

(defn conj-children
  [coll-node reader {:keys [:read-fn
                            :count-pred]
                     take-n :take-n}]
  (loop [i 0
         out []]
    (if (> i 10000)
      (do
        (js/console.error (js/Error. "Infinite loop?"))
        (valid-exit coll-node out))
      (if (and (some? take-n) (identical? i take-n))
        (valid-exit coll-node out)
        (let [next-node (read-fn reader)]
          (if (nil? next-node)
            (invalid-exit coll-node reader out)
            (let [tag (.-tag next-node)
                  value (.-value next-node)
                  children (.-children next-node)
                  next-i (if (and (some? take-n) (some? count-pred))
                           (cond-> i
                                   (count-pred next-node) (inc))
                           (inc i))]
              (case tag
                :unmatched-delimiter
                (if
                 (contains? (set *delimiter*) value)        ;; can match prev
                  (do
                    (unread reader value)
                    (invalid-exit coll-node reader out))
                  (recur next-i (conj out (report-invalid! next-node))))

                :splice
                (if take-n
                  (let [[valid? taken-values remaining-values] (split-after-n take-n count-pred nil children)]
                    (if valid?
                      (Splice (valid-exit coll-node taken-values)
                              remaining-values)
                      (invalid-exit coll-node reader (into out children))))
                  (recur next-i (into out children)))

                :eof
                (invalid-exit coll-node reader out)

                :matched-delimiter
                (if (and take-n (not= take-n i))
                  (do (unread reader value)
                      (invalid-exit coll-node reader out))
                  (valid-exit coll-node out))

                (recur next-i (conj out next-node))))))))))

(defn NodeWithChildren
  [reader read-fn tag delimiter]
  (r/read-char reader)
  (binding [*delimiter* (cons delimiter *delimiter*)]
    (conj-children (EmptyNode tag) reader {:read-fn read-fn})))

(defn read-string-data
  [node reader]
  (ignore reader)
  (loop [escape? false
         out ""]
    (if-let [c (r/read-char reader)]
      (cond (and (not escape?) (identical? c \"))
            (doto node (-> .-value
                           (set! out)))
            :else
            (recur (and (not escape?) (identical? c \\))
                   (perf/str out c)))
      (report-invalid!
       (assoc node :tag :token
                   :options {:tag (.-tag node)}
                   :value (str \" out))))))

(def non-breaking-space \u00A0)

(defn newline?
  [c]
  (perf/identical-in? [\newline
                       \return]
                      c))

(defn space?
  [c]
  (perf/identical-in? [\space
                       \tab
                       non-breaking-space]
                      c))

(defn whitespace?
  [c]
  (perf/identical-in? [\,
                       \space
                       \newline
                       \tab
                       non-breaking-space
                       \return]
                      c))

(defn brace? [ch]
  (perf/identical-in? [\( \) \[ \] \{ \} \"]
                      ch))

(defn prefix-boundary? [ch]
  (perf/identical-in? [\; \: \' \@ \^ \` \~ \\ nil]
                      ch))

(defn boundary? [ch]
  (or (brace? ch)
      (prefix-boundary? ch)))

(defn get-line [^Node node]
  (-> node .-range (nth 0)))