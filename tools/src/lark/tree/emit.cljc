(ns lark.tree.emit
  (:refer-clojure :exclude [*ns*])
  (:require [fast-zip.core :as z]
            [lark.tree.fn :refer [fn-walk]]
            [clojure.string :as string]
    #?@(:cljs [[cljs.tools.reader.edn :as edn]
               [cljs.tools.reader :as r]])
    #?@(:clj
        [
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader :as r]
            [lark.tree.util :refer [contains-identical-keyword?]]
            [lark.util.backtick :refer [template]]]))
  #?(:cljs (:require-macros [lark.util.backtick :refer [template]]
             [lark.tree.util :refer [contains-identical-keyword?]])))

(def ^:dynamic *ns* (symbol "lark.tree.user"))
(def ^:dynamic *features* #{:cljs})
(def ^:dynamic *print-selections* false)
(def ^:dynamic *indent-level* 0)
(def ^:dynamic *prettify* false)

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
    (cond (string/starts-with? x "def") :indent
          (re-find #"with|when|if" x) 1
          :else 0)))


(defn repeat-string [content n]
  (loop [i   0
         out ""]
    (if (= i n)
      out
      (recur (inc i) (str out content)))))

(def edges
  {:deref            ["@"]
   :list             [\( \)]
   :fn               ["#(" \)]
   :map              [\{ \}]
   :meta             ["^"]
   :quote            ["'"]
   :reader-meta      ["#^"]
   :raw-meta         ["^"]
   :reader-macro     ["#"]
   :regex            ["#\"" \"]
   :set              ["#{" \}]
   :string           [\" \"]
   :syntax-quote     ["`"]
   :unquote          ["~"]
   :unquote-splicing ["~@"]
   :uneval           ["#_"]
   :var              ["#'"]
   :vector           [\[ \]]
   ;:comment          [";"] ;; to avoid highlighting, we don't consider the leading ; an 'edge'

   })

(def tag-for-print-only? #{:comment :comment-block :uneval :space :newline :comma})

(def prefix-parent? (reduce (fn [s [k [_ rb]]]
                              (cond-> s
                                      (nil? rb) (conj k))) #{} edges))



(defn depth-inc [{:keys [tag value column] :as node} left-edge]
  (if (and (contains-identical-keyword? [:list :fn] tag)
           (= :symbol (-> value (first) (:tag))))
    (let [op          (-> (first value)
                          :value)
          indent-type (indentation-for op)]
      (case indent-type
        :indent (+ column (count left-edge) 1)
        (or (some-> (first (sequence (comp (filter #(not= (:tag %) :space))
                                           (take-while #(not= (:tag %) :newline))
                                           (drop (inc indent-type))) value))
                    (:column))
            (+ column (count left-edge) 1)
            (+ column (count left-edge) 1))))
    (+ column (count left-edge))))

(declare string)

(defn wrap-children [node left right children]
  (binding [*indent-level* (depth-inc node left)]
    (str left (apply str (mapv string children)) (when-not (= (:tag (last children))
                                                              :error/missing-delimiter)
                                                   right))))

#_(defn children? [{:keys [tag]}]
    (#{:list :fn :map :meta :set :vector :uneval} tag))
(defn string
  "Emit ClojureScript string from a magic-tree AST"
  ([node]
   (when-not (nil? node)
     (if (map? node)
       (let [{:keys [tag value prefix]} node
             [lbracket rbracket] (get edges tag [])]
         (if (= "error" (namespace tag))
           nil
           (case tag
             :base (apply str (mapv string value))
             :token value

             :comma value #_(if *prettify* "," value)
             :space value #_(if *prettify* " " value)
             :newline (if *prettify*
                        (str \newline (repeat-string " " *indent-level*))
                        value)

             :selection (when (some? *print-selections*)
                          (wrap-children node "‹" "›" value))
             :cursor (when (some? *print-selections*)
                       "|")
             (:deref
               :fn
               :list
               :map
               :quote
               :reader-macro
               :reader-conditional
               :set
               :syntax-quote
               :uneval
               :unquote
               :unquote-splicing
               :var
               :vector) (wrap-children node (str lbracket prefix) rbracket value)
             (:meta :reader-meta) (str prefix (wrap-children node lbracket rbracket value))
             (:string
               :regex) (str lbracket value rbracket)
             :comment (str \; value)
             :comment-block (string/join (sequence (comp (map #(if (.test #"^\s*$" %)
                                                                 %
                                                                 (str ";; " %)))
                                                         (interpose "\n"))
                                                   (string/split-lines value)))

             :keyword (str value)
             :namespaced-keyword (str "::" (some-> (namespace value) (str "/")) (name value))

             :symbol value

             nil "")))
       (string (z/node node)))))
  ([ns node]
   (binding [*ns* (or ns (symbol "cljs.user"))]
     (string node))))

(declare sexp)

(defn as-code [forms]
  (reduce (fn [out {:keys [tag splice?] :as item}]
            (if (tag-for-print-only? tag)
              out
              (let [value (sexp item)]
                ((if splice? into conj)
                  out value)))) [] forms))

(defn sexp [{:keys [tag value prefix] :as node}]
  (when node
    (if (= "error" (namespace tag))
      (throw (#?(:cljs js/Error.
                 :clj  Exception.) node))
      (case tag
        :base (as-code value)

        (:space
          :newline
          :comma
          :cursor) nil

        (:selection) (some-> (seq value) (as-code))

        :symbol (symbol value)
        :string value
        :deref (template (deref ~(first (as-code value))))

        :token (edn/read-string value)
        :vector (vec (as-code value))
        :list (list* (as-code value))
        :fn (fn-walk (as-code value))
        :map (apply hash-map (as-code value))
        :set (template #{~@(as-code value)})
        :var (template #'~(first (as-code value)))
        (:quote :syntax-quote) (template (quote ~(first (as-code value))))
        :unquote (template (~'clojure.core/unquote ~(first (as-code value))))
        :unquote-splicing (template (~'clojure.core/unquote-splicing ~(first (as-code value))))
        :reader-macro (r/read-string (string node))
        :reader-conditional (let [[feature form] (->> (remove #(tag-for-print-only? (:tag %)) (:value (first value)))
                                                      (partition 2)
                                                      (filter (fn [[{feature :value} _]] (contains? *features* feature)))
                                                      (first))]
                              (if feature
                                (cond-> (sexp form)
                                        (= prefix "#?") (vector))
                                []))
        (:meta
          :reader-meta) (let [[m data] (as-code value)]
                          (cond-> data
                                  #?(:cljs (satisfies? IWithMeta data)
                                     :clj  (instance? clojure.lang.IMeta data))
                                  (with-meta (if (map? m) m {m true}))))
        :regex (re-pattern value)
        :namespaced-keyword (keyword *ns* (name value))
        :keyword value

        (:comment
          :comment-block
          :uneval) nil))))
