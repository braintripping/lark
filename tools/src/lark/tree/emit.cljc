(ns lark.tree.emit
  (:refer-clojure :exclude [*ns*])
  (:require [fast-zip.core :as z]
            [lark.tree.fn :refer [fn-walk]]
            [clojure.string :as string]
            [lark.tree.format :as format]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.nav :as nav]
            [chia.util.perf :as perf]
            [chia.util :as u]
            #?@(:cljs [[cljs.tools.reader.edn :as edn]
                       [cljs.tools.reader :as r]])
            #?@(:clj
                [[clojure.tools.reader.edn :as edn]
                 [clojure.tools.reader :as r]
                 [lark.backtick.core :refer [template]]])
            [clojure.string :as str])
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

(def tag-for-print-only? #{:comment :comment-block :uneval :space :newline :comma})

(declare string*)

(defn last-line-length [s]
  (let [last-line-start (.lastIndexOf s \newline)]
    (when-not (identical? -1 last-line-start)
      (- (.-length s) last-line-start))))

(defn wrap-children [start-indent loc children]
  (let [node (.-node loc)
        tag (.-tag node)
        [left right] (rd/edges tag)]
    (if format/*pretty*
      (let [left-edge-width (or (some-> left .-length) 0)
            body-indent (+ left-edge-width (format/body-indent* start-indent loc 0))
            topline-indent (+ left-edge-width start-indent)
            end (count children)]
        (loop [out (or left "")
               current-indent topline-indent
               i 0]
          (if (= i end)
            (cond-> out
                    right (perf/str right))
            (let [child (nth children i)
                  tag (.. child -node -tag)]
              (if (perf/identical? :newline tag)
                (recur
                 (-> out
                     (perf/str \newline)
                     (perf/str (format/spaces body-indent)))
                 body-indent
                 (inc i))
                (if-let [child-str (string* current-indent child)]
                  (recur
                   (perf/str out child-str)
                   (if-let [child-length (some-> (last-line-length child-str)
                                                 (dec))]
                     child-length
                     (+ current-indent (.-length child-str)))
                   (inc i))
                  (recur out current-indent (inc i))))))))
      (-> (apply str (mapv #(string* start-indent %) children))
          (cond->> left (perf/str left))
          (cond-> right (perf/str right))))))

#_(defn children? [{:keys [tag]}]
    (#{:list :fn :map :meta :set :vector :uneval} tag))
(def log (volatile! {}))
(js/setTimeout #(->> @log
                     (seq)
                     (sort-by second)
                     (reverse)
                     (interpose \newline)
                     (print))
               2000)
(defn string*
  "Emit ClojureScript string from a magic-tree AST"
  [indent loc]
  (when (some? loc)
    (let [node (.-node loc)
          tag (.-tag node)
          value (.-value node)]
      (cond (perf/!keyword-identical? :token tag) value
            (perf/!keyword-identical? :space tag) (if format/*pretty*
                                                    (cond
                                                      (some-> rd/*active-cursor-node*
                                                              (= node)) value #_(format/spaces (min 2 (.-length value))) ;;value
                                                      (format/emit-space? loc) " ")
                                                    value)
            (perf/keyword-in? [:list :vector :map] tag) (wrap-children indent loc (nav/child-locs loc))
            :else
            (case tag
              :error nil
              :unmatched-delimiter value

              :base (wrap-children 0 loc (nav/child-locs loc))

              (:symbol
               :number) value

              :comma value
              :newline (if format/*pretty*
                         (perf/str \newline (format/spaces indent))
                         value)

              :selection (when *print-selections*
                           (wrap-children indent loc (nav/child-locs loc)))

              :cursor (when *print-selections* "|")

              :token value
              ;:space moved above
              (:list
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
               :reader-meta) (wrap-children indent loc (nav/child-locs loc))
              :namespaced-map (str (get (.-options node) :prefix)
                                   (wrap-children indent loc (nav/child-locs loc)))
              :string (str \" value \")
              :comment (str \; value)
              :comment-block (string/join (sequence (comp (map #(if (.test #"^\s*$" %)
                                                                  %
                                                                  (str ";; " %)))
                                                          (interpose "\n"))
                                                    (string/split-lines value)))

              :keyword (if (:resolve-ns? (.-options node))
                         (str "::" (some-> (namespace value) (str "/")) (name value))
                         (str value))

              nil "")))))

(defn string [loc]
  (string* 0
           (cond-> loc
                   (and loc
                        (not= (type loc) z/ZipperLocation))
                   (n/ast-zip))))

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

(defn sexp [node]
  (when node
    (let [tag (.-tag node)
          value (.-value node)
          children (.-children node)
          options (.-options node)]
      (if (perf/identical? :error tag)
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
          (let [[feature form] (->> (remove #(tag-for-print-only? (:tag %)) (:children (first children)))
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
