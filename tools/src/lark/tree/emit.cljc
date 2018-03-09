(ns lark.tree.emit
  (:refer-clojure :exclude [*ns*])
  (:require [fast-zip.core :as z]
            [lark.tree.fn :refer [fn-walk]]
            [clojure.string :as string]
            [lark.tree.format :as format]
            [lark.tree.reader :as rd]
            [lark.tree.node :as n]
            [lark.tree.nav :as nav]
   #?@(:cljs [[cljs.tools.reader.edn :as edn]
              [cljs.tools.reader :as r]])
   #?@(:clj
       [
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader :as r]
            [lark.tree.util :as util]
            [lark.backtick.core :refer [template]]]))
  #?(:cljs (:require-macros [lark.backtick.core :refer [template]]
            [lark.tree.util :as util])))

(def ^:dynamic *ns* (symbol "lark.tree.user"))
(def ^:dynamic *features* #{:cljs})
(def ^:dynamic *print-selections* false)

(def edges rd/edges)

(def bracket-match
  {\( \)
   \{ \}
   \[ \]
   \) \(
   \} \{
   \] \[})

(def tag-for-print-only? #{:comment :comment-block :uneval :space :newline :comma})

(declare string)

(defn wrap-children [loc children]
  (let [{:as node
         :keys [tag]} (z/node loc)
        [left right] (get rd/edges tag)
        threading? (and (some? right)                       ;; avoid threading-check as fast as possible
                        (= :list tag)
                        (some-> loc
                                (z/up)
                                (z/node)
                                (format/threading-node?)))]
    (binding [format/*indent-level* (format/child-depth* node (count left) (when threading?
                                                                             {:threading? threading?}))]
      (str left
           (apply str (mapv string children))
           right))))

#_(defn children? [{:keys [tag]}]
    (#{:list :fn :map :meta :set :vector :uneval} tag))
(defn string
  "Emit ClojureScript string from a magic-tree AST"
  ([loc]
   (when (some? loc)
     (if (= (type loc) z/ZipperLocation)
       (let [{:keys [tag value options] :as node} (z/node loc)
             children (nav/child-locs loc)]
         (if (= :error tag)
           nil
           (case tag
             :unmatched-delimiter value

             :base (apply str (mapv string children))

             :token value

             (:symbol
              :number) (str value)

             :comma value
             :space (if format/*prettify* " " value)
             :newline (if format/*prettify*
                        (str \newline (format/repeat-string format/INDENT format/*indent-level*))
                        value)

             :selection (when (some? *print-selections*)
                          (str "‹" (apply str (mapv string children)) "›"))

             :cursor (when (some? *print-selections*) "|")

             (:deref
              :fn
              :list
              :map
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
              :vector
              :regex) (wrap-children loc children)
             (:meta :reader-meta) (wrap-children loc children)
             :string (str \" value \")
             :comment (str \; value)
             :comment-block (string/join (sequence (comp (map #(if (.test #"^\s*$" %)
                                                                 %
                                                                 (str ";; " %)))
                                                         (interpose "\n"))
                                                   (string/split-lines value)))

             :keyword (if (:resolve-ns? options)
                        (str "::" (some-> (namespace value) (str "/")) (name value))
                        (str value))

             nil "")))
       (string (n/ast-zip loc)))))
  ([ns node]
   (binding [*ns* (or ns (symbol "cljs.user"))]
     (string node))))

(declare sexp)

(def splice? #{:reader-conditional-splice

               #_:unquote-splicing})


(defn as-code [forms]
  (reduce (fn [out {:keys [tag] :as item}]
            (if (tag-for-print-only? tag)
              out
              (let [value (sexp item)]
                ((if (contains? splice? tag) into conj)
                 out value)))) [] forms))

(defn sexp [{:keys [tag value children options] :as node}]
  (when node
    (if (= :error tag)
      (throw (#?(:cljs js/Error.
                 :clj  Exception.) node))
      (case tag
        :base (as-code children)

        (:space
         :newline
         :comma
         :cursor) nil

        (:selection) (some-> (seq children) (as-code))

        (:number :symbol) value

        :string value

        :unmatched-delimiter ::INVALID_TOKEN

        :deref (template (deref ~(first (as-code children))))

        :token (if (get options :invalid?)
                 ::INVALID_TOKEN
                 (edn/read-string value))

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
            (cond-> (sexp form)
                    (= tag :reader-conditional) (vector))
            []))

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

        (:comment
         :comment-block
         :uneval) nil))))
