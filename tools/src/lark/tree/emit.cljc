(ns lark.tree.emit
  (:refer-clojure :exclude [*ns*])
  (:require [fast-zip.core :as z]
            [lark.tree.fn :refer [fn-walk]]
            [clojure.string :as str]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.nav :as nav]
            [lark.tree.format :as format]
            [chia.util.perf :as perf]
            [chia.util :as u]
            #?@(:cljs [[cljs.tools.reader.edn :as edn]
                       [cljs.tools.reader :as r]])
            #?@(:clj
                [[clojure.tools.reader.edn :as edn]
                 [clojure.tools.reader :as r]
                 [lark.backtick.core :refer [template]]]))
  #?(:cljs (:require-macros [lark.backtick.core :refer [template]]
                            [lark.tree.util :as util])))

(def ^:dynamic *ns* (symbol "lark.tree.user"))
(def ^:dynamic *features* #{:cljs})
(def ^:dynamic *print-selections* false)

(def edges rd/edges)

(defn bracket-match [x]
  (case x \( \)
          \{ \}
          \[ \]
          \) \(
          \} \{
          \] \[
          nil))

(defn tag-for-print-only? [tag]
  (perf/keyword-in? [:space
                     :newline
                     :comma
                     :comment
                     :comment-block
                     :uneval]
                    tag))

(defn wrap-value [tag value]
  (let [[left right] (rd/edges tag)]
    (-> value
        (cond->> left (perf/str left))
        (cond-> right (perf/str right)))))

(declare sexp)

(def splice? #{:reader-conditional-splice

               #_:unquote-splicing})


(defn as-code [forms]
  (reduce (fn [out node]
            (if (tag-for-print-only? (.-tag node))
              out
              (let [value (sexp node)]
                (if (nil? value)
                  out
                  ((if (contains? splice? (.-tag node)) into conj)
                   out value))))) [] forms))

(declare materialize)

(defn- with-string [line column node string]
  [line (+ column (.-length string)) (assoc node :string string)])

(defn- with-string-multiline [line column node string]
  (let [lines-added (format/count-lines string)]
    [(+ line
        lines-added) (if (zero? lines-added)
                       (+ column (.-length string))
                       (dec (format/last-line-length string))) (assoc node :string string)]))

(defn- with-children [start-line start-column opts node edges]
  (let [tag (.-tag node)
        is-list? (perf/!keyword-identical? :list tag)
        operator (and is-list?
                      (some-> (first (.-children node))
                              (u/guard #(perf/!keyword-identical? :token (.-tag %)))
                              .-value))
        next-options {:parent (assoc node :range [start-line start-column])
                      :parent-op operator
                      :threading-form (and is-list?
                                           (some-> (:parent-op opts)
                                                   (u/guard #(str/ends-with? % "->"))))}
        [left right] edges
        column (if (some? left)
                 (+ start-column (.-length left))
                 start-column)
        [line column children-nodes children-str] (let [children (.-children node)
                                                        children (cond-> children
                                                                         (seq? children) (vec))
                                                        end (count children)
                                                        opts next-options]
                                                    (loop [i 0
                                                           line start-line
                                                           column column
                                                           children children
                                                           out-str (or left "")]
                                                      (if (identical? i end)
                                                        [line column children (cond-> out-str
                                                                                      (some? right) (perf/str right))]
                                                        (if-let [[line column node] (materialize line column (nth children i) opts i children)]
                                                          (recur (inc i)
                                                                 line
                                                                 column
                                                                 (assoc children i node)
                                                                 (perf/str out-str (.-string node)))
                                                          (recur (inc i)
                                                                 line
                                                                 column
                                                                 children
                                                                 out-str)))))
        column (cond-> column
                       (some? right) (+ (.-length right)))]
    [line column (assoc node :string children-str
                             :children children-nodes)]))

(defn- emit-space? [node i siblings]
  (and (not (zero? i))
       (not (identical? node (peek siblings)))
       (not (->> (dec i)
                 (nth siblings)
                 .-tag
                 (perf/!keyword-identical? :newline)))))

(defn- body-indent
  [parent-node siblings threading-form]
  (let [column (:column parent-node)
        tag (.-tag parent-node)
        inner-column (+ column
                        (some-> (rd/edges tag)
                                (first)
                                (.-length)))]
    (if-not (perf/!keyword-identical? :list tag)
      inner-column
      (let [operator (first siblings)]
        (if-not (perf/!keyword-identical? :token (some-> operator .-tag))
          inner-column
          (let [indent-type (format/indentation-for (name (.-value operator)))]
            (if (perf/!keyword-identical? :indent indent-type)
              (+ inner-column 1)
              (let [indent-offset (-> indent-type
                                      (cond-> threading-form (dec)))
                    split-after (+ 2 indent-offset)
                    [exact? taken _ num-passed] (->> (cond-> siblings
                                                             (n/whitespace? operator) (format/butlast-vec))
                                                     (rd/split-after-n split-after
                                                                       n/sexp?
                                                                       (fn [node]
                                                                         (perf/!keyword-identical? :newline (.-tag node)))))]
                (cond exact? (:column (last taken))
                      (and (identical? num-passed 1)
                           (not threading-form)) inner-column
                      :else (inc inner-column))))))))))

(defn materialize
  "Emit ClojureScript string from a magic-tree AST"
  ([node]
   (let [[line column node] (materialize 0 0 node nil nil nil)]
     node))
  ([line column node opts i siblings]
   (let [tag (.-tag node)
         ;_ (vswap! format/hist update tag inc)
         value (.-value node)
         with-children* #(with-children line column opts node (rd/edges tag))
         [end-line end-column node :as result]
         (perf/!condentical-keyword tag
           :token (with-string line column node value)

           :space (if format/*pretty*
                    (when (emit-space? node i siblings)
                      (with-string line column node " "))
                    (with-string line column node value))

           (:list :vector :map) (with-children*)

           :newline (if format/*pretty*
                      (let [indent (body-indent (get opts :parent) siblings (get opts :threading-form))]
                        [(inc line) indent (assoc node :string (str \newline (format/spaces indent)))])
                      [(inc line) (dec (.-length value)) (assoc node :string value)])

           (case tag
             :number (with-string line column node value)
             :string (with-string-multiline line column node (str "\"" value "\""))
             :keyword (with-string line column node (str (if (:resolve-ns? (.-options node)) "::" ":")
                                                         (some-> (namespace value) (str "/"))
                                                         (name value)))
             :error nil

             (:symbol
              :comma
              :token
              :space
              :unmatched-delimiter) (with-string line column node value)

             :comment (with-string line column node (str \; value))

             :comment-block (with-string-multiline line column node (str/join (sequence (comp (map #(if (.test #"^\s*$" %)
                                                                                                      %
                                                                                                      (str ";; " %)))
                                                                                              (interpose "\n"))
                                                                                        (str/split-lines value))))

             :cursor (when *print-selections*
                       (with-string line column node "|"))

             :selection (when *print-selections*
                          (with-children*))

             ;:space moved above
             (:base
              :list
              :vector
              :map

              :deref
              :fn
              :quote
              :reader-macro
              :reader-conditional
              :reader-conditional-splice
              :set
              :syntax-quote
              :uneval
              :unquote
              :unquote-splicing
              :var
              :regex
              :meta
              :reader-meta) (with-children*)
             :namespaced-map (with-children line column opts node [(get (.-options node) :prefix)])

             nil nil))]
     (when result
       [end-line end-column
        (assoc node :range [line column end-line end-column])]))))

(defn string [ast]
  (-> (cond-> ast (= (type ast) z/ZipperLocation)
              (.-node))
      (materialize)
      :string))

(defn sexp [node]
  (when node
    (let [tag (.-tag node)
          value (.-value node)
          children (.-children node)
          options (.-options node)]
      (if (perf/!keyword-identical? :error tag)
        (throw (#?(:cljs js/Error.
                   :clj  Exception.) node))
        (case tag
          :base (as-code children)

          (:space
           :newline
           :comma
           :cursor) nil



          (:selection) (some-> (seq children) (as-code))

          :string value

          :unmatched-delimiter ::INVALID_TOKEN

          :deref (template (deref ~(first (as-code children))))

          (:token
           :number) (try (edn/read-string value)
                         (catch js/Error e ::INVALID_TOKEN))

          :vector (vec (as-code children))

          :list (apply list (as-code children))

          :fn (fn-walk (first (as-code children)))

          :map (apply hash-map (as-code children))

          :set (template #{~@(as-code (:children (first children)))})

          :var (template #'~(first (as-code children)))

          (:quote :syntax-quote) (template (quote ~(first (as-code children))))

          :unquote (template (~'clojure.core/unquote ~(first (as-code children))))

          :unquote-splicing (template (~'clojure.core/unquote-splicing ~(first (as-code children))))

          :reader-macro (r/read-string (string node))

          (:reader-conditional
           :reader-conditional-splice)
          (let [[feature form] (->> (remove #(tag-for-print-only? (.-tag %)) (.-children (first children)))
                                    (partition 2)
                                    (filter (fn [[{feature :value} _]] (contains? *features* feature)))
                                    (first))]
            (if feature
              (sexp form)
              nil))

          (:meta
           :reader-meta) (let [[m data] (as-code children)]
                           (cond-> data
                                   #?(:cljs (satisfies? IWithMeta data)
                                      :clj  (instance? clojure.lang.IMeta data))
                                   (with-meta (if (map? m) m {m true}))))

          :regex (re-pattern (first (as-code children)))

          :keyword (if (:resolve-ns? options)
                     (let [resolved-ns (if-let [the-ns (namespace value)]
                                         (str (get r/*alias-map* (symbol the-ns) the-ns))
                                         *ns*)]
                       (keyword resolved-ns (name value)))
                     value)

          :namespaced-map (let [{:keys [namespace-name
                                        resolve-ns?]} options
                                m (sexp (first children))
                                map-ns (if resolve-ns?
                                         (if namespace-name
                                           (str (or (get r/*alias-map* (symbol namespace-name))
                                                    (throw (ex-info (str "Invalid value used as namespace in namespaced map: " namespace-name)
                                                                    {:options options}))))
                                           *ns*)
                                         namespace-name)]
                            (u/update-keys (fn [k]
                                             (if-not (keyword? k)
                                               k
                                               (let [the-ns (namespace k)]
                                                 (cond (nil? the-ns) (keyword map-ns (name k))
                                                       (= "_" the-ns) (keyword (name k))
                                                       :else k)))) m))

          (:comment
           :comment-block
           :uneval) nil)))))