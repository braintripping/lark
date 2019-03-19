(ns lark.tree.emit
  (:refer-clojure :exclude [*ns*])
  (:require [fast-zip.core :as z]
            [lark.tree.fn :refer [fn-walk]]
            [clojure.string :as str]
            [lark.tree.reader :as rd]
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

(defn- emit-space? [prev-siblings more-siblings?]
  (and more-siblings?
       (not (zero? (count prev-siblings)))
       (not (->> (peek prev-siblings)
                 .-tag
                 (perf/unchecked-keyword-identical? :newline)))))

(defn- add-space? [t1 t2]
  (and (not (perf/keyword-in? [:space :newline :data-literal :_] t1))
       (not (perf/keyword-in? [:space :newline] t2))))

(def space-node (rd/ValueNode :space " "))

(defn get-operator [tag children child-count]

  (when (and (not (zero? child-count))
             (some? tag)
             (perf/unchecked-keyword-identical? :list tag))
    (some-> (first children)
            (u/guard #(perf/keyword-in? [:keyword :token] (.-tag %)))
            .-value)))

(defn- with-children [line column options node edges]
  (let [tag (.-tag node)
        children (.-children node)
        ;; note - may need to use special zipper operations to ensure that
        ;; children is always a vector
        children (if (vector? children)
                   children
                   (vec children))
        children-count (-count children)
        options {:parent (assoc node :range [line column])
                 :parent-op (get-operator tag children children-count)
                 :grandparent-op (:parent-op options)}
        [left right] edges
        left (or left "")
        right (or right "")]
    (loop [i 0
           line line
           column (+ column (.-length left))
           children-out []
           string-out left
           last-tag :_]
      (if (identical? i children-count)
        [line
         (+ column (.-length right))
         (assoc node :children children-out
                     :string (perf/str string-out right))]
        (let [node (nth children i)
              tag (.-tag node)
              more-siblings? (not (identical? i (dec children-count)))]
          (if (and format/*pretty*
                   (add-space? last-tag tag))
            (recur i
                   line
                   (inc column)
                   (conj children-out space-node)
                   (perf/str string-out " ")
                   :space)
            (if-let [[line column next-node] (materialize line
                                                          column
                                                          node
                                                          options
                                                          children-out
                                                          more-siblings?)]

              (recur (inc i)
                     line
                     column
                     (conj children-out next-node)
                     (perf/str string-out (.-string next-node))
                     tag)
              (recur (inc i)
                     line
                     column
                     children-out
                     string-out
                     last-tag))))))))

(defn report-selections! [tag node]
  (case tag :cursor (vreset! format/*cursor* (select-keys node [:line
                                                                :column]))
            :selection (vswap! format/*selections* conj node)
            nil))

(defn materialize
  "Emit ClojureScript string from a magic-tree AST"
  ([node]
   (binding [format/*cursor* (volatile! nil)
             format/*selections* (volatile! [])]
     (let [[line column node] (materialize 0 0 node nil nil nil)]
       (assoc node
         :ast/cursor-pos @format/*cursor*
         :ast/selections @format/*selections*))))
  ([node {:as opts
          :keys [format]}]
   (binding [format/*pretty* (boolean format)]
     (materialize node)))
  ([line column node opts prev-siblings more-siblings?]
   (let [tag (.-tag node)
         value (.-value node)
         with-children* #(with-children line column opts node (rd/edges tag))
         [end-line end-column node]
         (case tag
           :space (if (and format/*pretty*
                           (not (rd/active-cursor-line? line)))
                    (when (emit-space? prev-siblings more-siblings?)
                      (with-string line column node " "))
                    (with-string line column node value))

           :newline (if (and format/*pretty*
                             (not (rd/active-cursor-line? (inc line))))
                      (let [indent (format/body-indent opts prev-siblings)]
                        [(inc line) indent (assoc node :string (str \newline (format/spaces indent)))])
                      [(inc line) (dec (.-length value)) (assoc node :string value)])
           :number (with-string line column node value)

           :string (with-string-multiline line column node (str "\"" value "\""))
           :keyword (with-string line column node (str (if (:resolve-ns? (.-options node)) "::" ":")
                                                       (some-> (namespace value) (str "/"))
                                                       (name value)))
           :error nil

           (:symbol
            :token
            :invalid-token
            :comma
            :unmatched-delimiter) (with-string line column node value)

           :comment (with-string line column node (str \; value))

           :comment-block (with-string-multiline line column node (str/join (sequence (comp (map #(if (.test #"^\s*$" %)
                                                                                                    %
                                                                                                    (str ";; " %)))
                                                                                            (interpose "\n"))
                                                                                      (str/split-lines value))))

           :cursor (when *print-selections*
                     (with-string line column node "|"))

           :selection (if *print-selections*
                        (with-children*)
                        (with-children line column opts node nil))

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
            :reader-meta
            :data-literal) (with-children*)
           :namespaced-map (with-children line column opts node [(get (.-options node) :prefix)])
           nil)]
     (when node
       (let [node (assoc node :range [line column end-line end-column])]
         (report-selections! tag node)
         [end-line end-column node])))))

(defn string [ast]
  (-> (cond-> ast
              (= (type ast) z/ZipperLocation) (z/node))
      materialize
      :string))

(defn sexp [node]
  (when node
    (let [tag (.-tag node)
          value (.-value node)
          children (.-children node)
          options (.-options node)]
      (if (perf/unchecked-keyword-identical? :error tag)
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

          ;; TODO
          ;; data literals

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
                            (u/update-keys m (fn [k]
                                               (if-not (keyword? k)
                                                 k
                                                 (let [the-ns (namespace k)]
                                                   (cond (nil? the-ns) (keyword map-ns (name k))
                                                         (= "_" the-ns) (keyword (name k))
                                                         :else k))))))

          (:comment
           :comment-block
           :uneval) nil)))))
