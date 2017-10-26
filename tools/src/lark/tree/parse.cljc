;; modified from https://github.com/rundis/rewrite-cljs
;; https://github.com/rundis/rewrite-cljs/blob/master/LICENSE

(ns lark.tree.parse
  (:require [lark.tree.reader :as rd]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [clojure.string :as string]
    #?@(:cljs
        [[cljs.tools.reader.reader-types :as r]
         [cljs.tools.reader.edn :as edn]])
    #?@(:clj
        [
            [clojure.tools.reader.reader-types :as r]
            [clojure.tools.reader.edn :as edn]
            [lark.tree.util :refer [contains-identical-keyword?]]]))
  #?(:cljs (:require-macros [lark.tree.util :refer [contains-identical-keyword? contains-identical?]])))

#?(:cljs (enable-console-print!))

(def ^:dynamic *errors* nil)
(defn error! [info]
  (when (some? *errors*)
    (set! *errors* (conj *errors* info)))
  info)

(def ^:dynamic ^:private *delimiter* nil)
(declare parse-next)
(def non-breaking-space \u00A0)

;; identical? lookups are 10x faster than set-contains and 2x faster than js-array indexOf

(defn newline?
  [c]
  (contains-identical? [\newline
                        \return]
                       c))

(defn space?
  [c]
  (contains-identical? [\space
                        \tab
                        non-breaking-space]
                       c))

(defn whitespace?
  [c]
  (or (contains-identical? [\,
                            \space
                            \tab
                            non-breaking-space]
                           c)
      (newline? c)))

(defn boundary?
  [c]
  "Check whether a given char is a token boundary."
  (contains-identical? [\" \: \; \' \@ \^ \` \~ \( \) \[ \] \{ \} \\ nil]
                       c))

(defn read-to-boundary
  [reader allowed]
  (rd/read-until
    reader
    #(and (or (whitespace? %)
              (boundary? %))
          (not (allowed %)))))

(defn read-to-char-boundary
  [reader]
  (let [c (r/read-char reader)]
    (str c
         (if (not (identical? c \\))
           (read-to-boundary reader #{})
           ""))))

(defn dispatch
  [c]
  (cond (identical? c *delimiter*) :matched-delimiter
        (nil? c) :eof

        (identical? c \,) :comma

        (or (identical? c \space)
            (identical? c non-breaking-space)
            (identical? c \tab)) :space

        (newline? c) :newline

        (identical? c \^) :meta
        (identical? c \#) :sharp
        (identical? c \() :list
        (identical? c \[) :vector
        (identical? c \{) :map

        (contains-identical? [\}
                              \]
                              \)] c) :unmatched-delimiter

        (identical? c \~) :unquote
        (identical? c \') :quote
        (identical? c \`) :syntax-quote
        (identical? c \;) :comment
        (identical? c \@) :deref
        (identical? c \") :string
        (identical? c \:) :keyword
        :else :token))

(defn parse-delim
  [reader delimiter]
  (r/read-char reader)
  (rd/read-repeatedly reader #(binding [*delimiter* delimiter]
                                (parse-next %))))

(defn printable-only? [n]
  (contains-identical-keyword? [:space :comma :newline :comment :comment-block]
                               (:tag n)))

(defn parse-printables
  [reader node-tag n & [ignore?]]
  (when-not (nil? ignore?)
    (r/read-char reader))
  (rd/read-n
    reader
    node-tag
    parse-next
    (complement printable-only?)
    n))

(def brackets {\( \)
               \[ \]
               \{ \}})

(defn parse-token
  "Parse a single token."
  [reader]
  (let [first-char (r/read-char reader)
        s (->> (if (identical? first-char \\)
                 (read-to-char-boundary reader)
                 (read-to-boundary reader #{}))
               (str first-char))
        ;; determine if string is a symbol, inferring 'yes' on a
        ;; symbol-related read error:
        sexp (try (edn/read-string s)
                  (catch #?(:cljs js/Error
                            :clj  Exception) e
                    (let [message #?(:cljs (ex-message e)
                                     :clj  (.getMessage e))]
                      ;; TODO
                      ;; a better way to determine that this is an invalid symbol?
                      (when (string/ends-with? message "/.")
                        ::invalid-symbol))))
        is-symbol (or (symbol? sexp) (= sexp ::invalid-symbol))]
    (if is-symbol
      [:symbol (str s (read-to-boundary reader #{\' \:}))]
      [:token s])))

(defn parse-keyword
  [reader]
  (r/read-char reader)
  (if-let [c (r/peek-char reader)]
    (if (identical? c \:)
      [:namespaced-keyword (edn/read reader)]
      (do (r/unread reader \:)
          ;; TODO
          ;; handle invalid keywords
          [:keyword (edn/read reader)]))
    (rd/throw-reader reader "unexpected EOF while reading keyword.")))

(defn parse-sharp
  [reader]
  (r/read-char reader)
  (case (r/peek-char reader)
    nil (rd/throw-reader reader "Unexpected EOF.")
    \{ [:set (parse-delim reader \})]
    \( [:fn (parse-delim reader \))]
    \" [:regex (rd/read-string-data reader)]
    \^ [:reader-meta (parse-printables reader :reader-meta 2 true)]
    \' [:var (parse-printables reader :var 1 true)]
    \_ [:uneval (parse-printables reader :uneval 1 true)]
    \? (do
         (r/read-char reader)
         (let [read-next #(parse-printables reader :reader-macro 1)
               opts (case (r/peek-char reader)
                      \( {:prefix  "#?"
                          :splice? true}
                      \@ (do (r/read-char reader)
                             {:prefix  "#?@"
                              :splice? true})
                      ;; no idea what this would be, but its \? prefixed
                      (do (rd/unread reader \?)
                          {:prefix (str "#?" (read-next))}))
               value (read-next)]
           [:reader-conditional value opts]))
    [:reader-macro (parse-printables reader :reader-macro 2)]))

(defn parse-unquote
  [reader]
  (r/read-char reader)
  (let [c (r/peek-char reader)]
    (if (identical? c \@)
      [:unquote-splicing (parse-printables reader :unquote 1 true)]
      [:unquote (parse-printables reader :unquote 1)])))

(defn parse-next*
  [reader]
  (let [c (r/peek-char reader)
        tag (dispatch c)]
    (case tag
      :token (parse-token reader)
      :keyword (parse-keyword reader)
      :sharp (parse-sharp reader)
      :comment (do (rd/ignore reader)
                   [tag (let [content (rd/read-until reader (fn [x] (or (nil? x) (#{\newline \return} x))))]
                          content)])
      (:deref
        :quote
        :syntax-quote) [tag (parse-printables reader tag 1 true)]

      :unquote (parse-unquote reader)

      :newline (do (rd/ignore reader)
                   [tag (str "\n" (rd/read-while reader space?))])

      :comma [tag (rd/read-while reader #(identical? % c))]
      :space [tag (rd/read-while reader space?)]
      (:list
        :vector
        :map) [tag (parse-delim reader (get brackets c))]

      :matched-delimiter (do (r/read-char reader) nil)
      (:eof :unmatched-delimiter) (let [the-error (error! [(keyword "error" (name tag)) (let [[line col] (rd/position reader)]
                                                                                          {:position  {:line       line
                                                                                                       :column     col
                                                                                                       :end-line   line
                                                                                                       :end-column (inc col)}
                                                                                           :delimiter *delimiter*})])]
                                    (r/read-char reader)
                                    the-error)
      :meta (do (r/read-char reader)
                [tag (parse-printables reader :meta 2)])
      :string [tag (rd/read-string-data reader)])))

(defn parse-next
  [reader]
  (rd/read-with-position reader parse-next*))

(defn indexing-reader
  "Create reader for strings."
  [s]
  (r/indexing-push-back-reader
    (r/string-push-back-reader s )))

(defn comment-block-child? [{:keys [tag]}]
  (contains-identical-keyword? [:space :newline :comment] tag))

(defn ast*
  [s]
  (binding [*errors* []]
    (loop [reader (indexing-reader s)
           source ""
           values []]
      (if-some [next-thing (rd/read-with-position reader parse-next*)]
        (let [next-source (emit/string next-thing)]
          (recur reader
                 (str source next-source)
                 (conj values (assoc next-thing :source next-source))))
        {:value      values
         :source     source
         :tag        :base
         :errors     *errors*
         :line       0
         :column     0
         :end-line   (r/get-line-number reader)
         :end-column (r/get-column-number reader)}))))

(defn ast
  "Parse ClojureScript source code to AST"
  ([source] (ast nil source))
  ([ns source]
   (let [{out-str :source :as the-ast} (ast* source)
         modified-source? (not= source out-str)
         result (assoc (if modified-source?
                         (ast* out-str)
                         the-ast) :string out-str
                                  :modified-source? modified-source?)]
     result)))

(defn normalize-comment-line [s]
  (string/replace s #"^;+\s?" ""))



(comment

  ;; IN PROGRESS
  ;; thinking about a better way to group comment and code blocks
  ;; ...contemplating a transducer, or similar thing?

  (defn conj-while [[out in] xform]
    (loop [out out
           in in]
      (if-let [form (xform (peek in))]
        (recur (update-in out [(dec (count out)) :value] conj form)
               (subvec in 1))
        [out in])))

  (groups {:comment-block {:init {:tag   :comment-block
                                  :value ""}
                           :pred comment-block-child?
                           :conj (fn [oldval node]
                                   (str oldval (-> (emit/string node)
                                                   (normalize-comment-line))))}
           :code-block    {:init {:tag   :base
                                  :value []}
                           :pred (complement comment-block-child?)
                           :conj (fn [oldval node]
                                   (conj oldval node))}} nodes))

(defn group-comment-blocks
  "Put consecutive top-level whitespace and comment nodes into :comment-blocks"
  [ast]
  (update ast :value (fn [nodes]
                       (reduce (fn [out node]
                                 (if (comment-block-child? node)
                                   (if (= :comment-block (:tag (peek out)))
                                     (update-in out [(dec (count out)) :value] str (-> (emit/string node)
                                                                                       (normalize-comment-line)))
                                     (conj out (merge node
                                                      {:tag   :comment-block
                                                       :value (normalize-comment-line (emit/string node))})))
                                   (conj out node))) [] nodes))))

(defn shape [{:keys [tag value] :as node}]
  (if (= tag :base)
    (mapv shape value)
    (if (n/may-contain-children? node)
      [tag (mapv shape value)]
      tag)))

(comment
  (let [s "\n;a\n;b\n\n;c\nd\ne\n;e\n"]
    (prn s)
    (prn (emit/string (ast s)))
    (= s (emit/string (ast s)))))
