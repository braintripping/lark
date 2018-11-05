(ns lark.tree.emit
  (:refer-clojure :exclude [*ns*])
  (:require [fast-zip.core :as z]
            [lark.tree.fn :refer [fn-walk]]
            [clojure.string :as str]
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


(declare string*)

(defn wrap-children [outer-indent node]
  (let [tag (.-tag node)
        [left right] (rd/edges tag)
        body-str (str/join (mapv #(string* outer-indent %) (.-children node)))]
    (-> body-str
        (cond->> left (perf/str left))
        (cond-> right (perf/str right)))))

(defn string*
  "Emit ClojureScript string from a magic-tree AST"
  [indent node]
  (when (some? node)
    (let [tag (.-tag node)
          value (.-value node)]
      (cond (perf/!keyword-identical? :token tag) value
            (perf/!keyword-identical? :space tag) value
            (perf/keyword-in? [:list :vector :map] tag) (wrap-children indent node)
            :else
            (case tag
              :error nil
              :unmatched-delimiter value

              :base (wrap-children 0 node)

              (:symbol
               :number) value

              :comma value
              :newline value

              :selection (when *print-selections*
                           (wrap-children indent node))

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
               :reader-meta) (wrap-children indent node)
              :namespaced-map (str (get (.-options node) :prefix)
                                   (wrap-children indent node))
              :string (str \" value \")
              :comment (str \; value)
              :comment-block (str/join (sequence (comp (map #(if (.test #"^\s*$" %)
                                                               %
                                                               (str ";; " %)))
                                                       (interpose "\n"))
                                                 (str/split-lines value)))

              :keyword (if (:resolve-ns? (.-options node))
                         (str "::" (some-> (namespace value) (str "/")) (name value))
                         (str value))

              nil "")))))

(defn string [ast]
  (string* 0
           (cond-> ast (= (type ast) z/ZipperLocation)
                   (.-node))))

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

(defn ast
  "Adds emitted source to ast root + top-level nodes"
  [base]
  (let [[source children]
        (reduce (fn [[source values] node]
                  (let [node-str (string* 0 node)]
                    [(str source node-str)
                     (conj! values
                            (assoc node :string node-str))]))
                ["" (transient [])] (.-children base))]
    (-> base
        (assoc :string source
               :children (persistent! children)))))

(defn zip [loc]
  (-> loc
      .-node
      (ast)
      (n/ast-zip)))

(comment
 (declare string*-old)

 (defn wrap-children-old [outer-indent loc children]
   (let [node (.-node loc)
         tag (.-tag node)
         [left right] (rd/edges tag)
         body-str (if-not format/*pretty*
                    (str/join (mapv #(string*-old outer-indent %) children))
                    (let [left-edge-width (if left
                                            (.-length left)
                                            0)
                          inner-indent (+ left-edge-width outer-indent)
                          body-indent (+ left-edge-width (format/body-indent* outer-indent loc))
                          child-count (count children)]
                      (loop [out ""
                             current-indent inner-indent
                             i 0]
                        (if (identical? i child-count)
                          out
                          (let [child (nth children i)
                                tag (.. child -node -tag)]
                            (if (perf/identical? :newline tag)
                              (recur
                               (-> out
                                   (perf/str \newline)
                                   (perf/str (format/spaces body-indent)))
                               body-indent
                               (inc i))
                              (if-let [child-str (string*-old current-indent child)]
                                (recur
                                 (perf/str out child-str)
                                 (if-let [child-length (some-> (format/last-line-length child-str)
                                                               (dec))]
                                   child-length
                                   (+ current-indent (.-length child-str)))
                                 (inc i))
                                (recur out current-indent (inc i)))))))))]
     (-> body-str
         (cond->> left (perf/str left))
         (cond-> right (perf/str right)))))


 (defn string*-old
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
             (perf/keyword-in? [:list :vector :map] tag) (wrap-children-old indent loc (nav/child-locs loc))
             :else
             (case tag
               :error nil
               :unmatched-delimiter value

               :base (wrap-children-old 0 loc (nav/child-locs loc))

               (:symbol
                :number) value

               :comma value
               :newline (if format/*pretty*
                          (perf/str \newline (format/spaces indent))
                          value)

               :selection (when *print-selections*
                            (wrap-children-old indent loc (nav/child-locs loc)))

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
                :reader-meta) (wrap-children-old indent loc (nav/child-locs loc))
               :namespaced-map (str (get (.-options node) :prefix)
                                    (wrap-children-old indent loc (nav/child-locs loc)))
               :string (str \" value \")
               :comment (str \; value)
               :comment-block (str/join (sequence (comp (map #(if (.test #"^\s*$" %)
                                                                %
                                                                (str ";; " %)))
                                                        (interpose "\n"))
                                                  (str/split-lines value)))

               :keyword (if (:resolve-ns? (.-options node))
                          (str "::" (some-> (namespace value) (str "/")) (name value))
                          (str value))

               nil "")))))

 (defn string-old [loc]
   (string*-old 0
                (cond-> loc
                        (and loc
                             (not= (type loc) z/ZipperLocation))
                        (n/ast-zip)))))