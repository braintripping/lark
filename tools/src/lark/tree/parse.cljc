;; modified from https://github.com/rundis/rewrite-cljs
;; https://github.com/rundis/rewrite-cljs/blob/master/LICENSE

(ns lark.tree.parse
  (:require [lark.tree.reader :as rd]
            [lark.tree.emit :as emit]
            [lark.tree.impl.parse :as impl]
            [lark.tree.util :as util]
            [cljs.tools.reader.impl.commons :refer [parse-symbol]]
            [chia.util.perf :as perf]
            [clojure.tools.reader.reader-types :as r]
            [clojure.tools.reader.edn :as edn]))

(declare parse-next read-token*)

(defn read-sharp
  [reader tag ch]
  (r/read-char reader)
  (case (r/peek-char reader)
    nil (rd/InvalidToken! :reader-macro "#")
    "{" (impl/read-n-children reader :set "#" 1 :map parse-next)
    "(" (impl/read-n-children reader :fn "#" 1 :list parse-next)
    "\"" (impl/read-n-children reader :regex "#" 1 :string parse-next)
    "^" (do (r/read-char reader)
            (impl/read-n-children reader :reader-meta "#^" 1 nil parse-next))
    "'" (do (r/read-char reader)
            (impl/read-n-children reader :var "#'" 1 :token parse-next))
    "_" (do (r/read-char reader)
            (impl/read-n-children reader :uneval "#_" 1 nil parse-next))
    "?" (do
          (r/read-char reader)
          (let [ch (r/peek-char reader)]
            (if-let [tag (case ch
                           "(" :reader-conditional
                           "@" (do (r/read-char reader)
                                   :reader-conditional-splice)
                           nil)]
              (impl/read-n-children reader tag (str "#?" (when (= ch "@" "@"))) 1 :list parse-next)
              (rd/InvalidToken! :reader-macro "#?"))))
    ":" (let [ch (r/read-char reader)
              resolve-ns? (= \: (r/peek-char reader))
              _ (when resolve-ns?
                  (r/read-char reader))
              token (read-token* reader ch)
              anonymous? (= token ":")
              expr (when-not anonymous?
                     (try (edn/read-string token)
                          (catch js/Error e
                            ::error)))
              prefix (str "#" (when resolve-ns? ":") token)
              options {:resolve-ns? resolve-ns?
                       :namespace-name (some-> expr (name))
                       :prefix prefix}]
          (cond (perf/identical? :error expr) (rd/InvalidToken! :namespaced-map prefix)
                (and (keyword? expr) (namespace expr)) (rd/InvalidToken! :namespaced-map prefix)
                :else
                (-> (impl/read-n-children reader :namespaced-map prefix 1 :map parse-next)
                    (assoc :options options))))
    (impl/read-n-children reader :data-literal "#" 1 :token parse-next)))

(defn read-unquote
  [reader tag ch]
  (r/read-char reader)
  (let [c (r/peek-char reader)]
    (if (identical? c \@)
      (do (r/read-char reader)
          (impl/read-printable-children reader :unquote-splicing 1 parse-next))
      (impl/read-printable-children reader :unquote 1 parse-next))))

(defn read-prefixed-1 [reader tag ch]
  (impl/read-n-children reader tag (r/read-char reader) 1 nil parse-next))

(defn read-eof [reader tag ch]
  nil)

(defn read-meta [reader tag ch]
  (do (r/read-char reader)
      (impl/read-printable-children reader tag 1 parse-next)))

(defn read-coll [reader tag ch]
  (rd/NodeWithChildren reader parse-next tag (emit/bracket-match ch)))

(defn read-matched [reader tag ch]
  (rd/ValueNode tag (r/read-char reader)))

(defn read-unmatched [reader tag ch]
  (-> (rd/ValueNode tag (r/read-char reader))
      (assoc :info {:direction :backward
                    :expects (emit/bracket-match ch)})))

;;;;;;;;;;;;;;;;;;;;
;;
;; Terminals/Read-Within

(defn re-test [re s]
  (.test re s))

(defn read-within [tag s]
  (case tag
    :token nil
    :comment nil
    :newline nil
    :space nil
    :keyword nil
    :string nil))

;;;;;;;;;;;;;;;;;;;;
;;
;; Terminals/Read

(defn ^string read-token* [rdr initial-ch]
  ;; adapted from cljs.tools.reader.edn
  (loop [out ""
         ch initial-ch]
    (cond (or (rd/whitespace? ch)
              (impl/macro-terminating? ch)) (do (r/unread rdr ch) out)

          (nil? ch) out

          :else
          (recur (perf/str out ch) (r/read-char rdr)))))

(defn- ^boolean number-string? [s]
  (not (js/isNaN (js/parseInt (subs s 0 1)))))

(defn read-token
  "Parse a single token."
  [reader tag ch]
  (r/read-char reader)
  (let [token (if (identical? ch "\\")
                (js* "~{} += ~{}" ch (impl/read-to-char-boundary reader))
                (read-token* reader ch))
        tag (if (number-string? token)
              :number
              :token)]
    (rd/ValueNode tag token)))

(defn read-keyword
  [reader tag ch]
  (let [ch (r/read-char reader)
        resolve-ns? (= \: (r/peek-char reader))
        _ (when resolve-ns?
            (r/read-char reader))
        token (read-token* reader ch)
        expr (try (edn/read-string token)
                  (catch :default e ::error))]
    (if (= expr ::error)
      (rd/InvalidToken! tag
                        (cond->> token
                                 resolve-ns? (str ":")))
      (rd/->Node tag
                 (when resolve-ns?
                   {:resolve-ns? resolve-ns?}) nil expr nil nil))))

(defn read-comment [reader tag ch]
  (do (r/read-char reader)
      (rd/ValueNode :comment (rd/read-until reader
                                            (fn [x]
                                              (or (nil? x) (#{\newline \return} x)))))))

(defn read-newline [reader tag ch]
  (do (r/read-char reader)
      (rd/ValueNode tag (str "\n"
                             (rd/read-while reader rd/space?)))))

(defn read-comma [reader tag ch]
  (rd/ValueNode tag (rd/read-while reader rd/comma?)))

(defn read-space [reader tag ch]
  (rd/ValueNode tag (rd/read-while reader rd/space?)))


(defn- read-string*
  [reader]
  (r/read-char reader)
  (loop [escape? false
         out ""]
    (if-let [c (r/read-char reader)]
      (cond (and (not escape?) (identical? c \"))
            [true out]
            :else
            (recur (and (not escape?) (identical? c \\))
                   (perf/str out c)))
      [false out])))

(defn read-string [reader tag ch]
  (let [[valid? s] (read-string* reader)]
    (if valid?
      (rd/ValueNode :string s)
      (rd/report-invalid!
       (-> (rd/ValueNode :token (str "\"" s))
           (assoc :options {:tag :string}))))))

(defn next-tag
  [c]
  (cond (nil? c) :eof
        (identical? (first rd/*delimiter*) c) :matched-delimiter
        :else
        (case c
          (\u00A0
           \space
           \tab) :space
          (\newline
           \return) :newline
          \( :list
          \[ :vector
          \" :string
          \^ :meta
          \: :keyword
          \; :comment
          \{ :map
          \# :sharp
          \@ :deref
          \, :comma
          \' :quote
          \~ :unquote
          \` :syntax-quote
          (\} \] \)) :unmatched-delimiter
          :token)))

(defn parser [tag]
  (case tag
    :token read-token
    :keyword read-keyword
    :sharp read-sharp
    :comment read-comment
    (:deref
     :quote
     :syntax-quote) read-prefixed-1
    :unquote read-unquote
    :newline read-newline
    :comma read-comma
    :space read-space
    (:list
     :vector
     :map) read-coll
    :matched-delimiter read-matched
    :unmatched-delimiter read-unmatched
    :eof read-eof
    :meta read-meta
    :string read-string))

(defn parse-next*
  [reader]
  (let [c (r/peek-char reader)
        tag (next-tag c)
        parse (parser tag)
        node (parse reader tag c)]
    node))

(defn parse-next
  [reader]
  (rd/read-with-position reader parse-next*))

(defn parse-base [reader]
  (rd/conj-children reader
                    (rd/EmptyNode :base)
                    {:read-fn parse-next}))

(defn ast
  [s]
  (binding [rd/*invalid-nodes* (volatile! [])]
    (-> (rd/indexing-reader s)
        (rd/read-with-position parse-base)
        (assoc! :invalid-nodes (util/guard-> @rd/*invalid-nodes* seq)))))
