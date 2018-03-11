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
            [lark.backtick.core :refer [template]]])
            [clojure.string :as str])
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

(defn wrap-children [start-indent loc children]
  (let [{:as coll-node :keys [tag]} (z/node loc)
        [left right] (get rd/edges tag)]
    (if-not format/*pretty*
      (str left (apply str (mapv (partial string start-indent) children)) right)
      (let [coll-edge-indent (count left)
            body-indent (+ coll-edge-indent (format/body-indent* start-indent loc 0))
            topline-indent (+ coll-edge-indent start-indent)]
        (str left (loop [out ""
                         current-indent topline-indent
                         remaining children]
                    (if (empty? remaining)
                      out
                      (let [child (first remaining)
                            {:keys [tag] :as child-node} (z/node child)]
                        (case tag
                          :newline
                          (recur
                           (str out \newline (format/repeat-string format/INDENT body-indent))
                           body-indent
                           (rest remaining))
                          (let [child-str (string current-indent child)
                                child-multiline? (some-> child-str
                                                         (str/includes? \newline))]
                            (recur
                             (str out child-str)
                             (if child-multiline? (let [last-line (re-find #"[^\n]*$" child-str)]
                                                    (count last-line))
                                                  (+ current-indent (count child-str)))
                             (rest remaining)))))))
             right)))))

#_(defn children? [{:keys [tag]}]
    (#{:list :fn :map :meta :set :vector :uneval} tag))
(defn string
  "Emit ClojureScript string from a magic-tree AST"
  ([indent loc]
   (when (some? loc)
     (if (= (type loc) z/ZipperLocation)
       (let [{:keys [tag value options] :as node} (z/node loc)
             children (nav/child-locs loc)]
         (if (= :error tag)
           nil
           (case tag
             :unmatched-delimiter value

             :base (wrap-children 0 loc children)

             :token value

             (:symbol
              :number) (str value)

             :comma value
             :space (if format/*pretty*
                      (cond (some-> rd/*active-cursor-node*
                                    (= node)) value
                            (format/emit-space? loc) " ")
                      value)
             :newline (if format/*pretty*
                        (str \newline (format/repeat-string format/INDENT indent))
                        value)

             :selection (when (some? *print-selections*)
                          (wrap-children indent loc children))

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
              :regex) (wrap-children indent loc children)
             (:meta :reader-meta) (wrap-children indent loc children)
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
       (string indent (n/ast-zip loc)))))
  ([loc]
   (string 0 loc)))

(declare sexp)

(def splice? #{:reader-conditional-splice

               #_:unquote-splicing})


(defn as-code [forms]
  (reduce (fn [out {:keys [tag] :as item}]
            (if (tag-for-print-only? tag)
              out
              (let [value (sexp item)]
                (if (nil? value)
                  out
                  ((if (contains? splice? tag) into conj)
                   out value))))) [] forms))

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

        (:comment
         :comment-block
         :uneval) nil))))
