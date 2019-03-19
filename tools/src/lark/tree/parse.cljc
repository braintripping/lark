;; modified from https://github.com/rundis/rewrite-cljs
;; https://github.com/rundis/rewrite-cljs/blob/master/LICENSE

(ns lark.tree.parse
  (:require [lark.tree.reader :as rd]
            [lark.tree.emit :as emit]
            [lark.tree.format]
            [cljs.tools.reader.impl.commons :refer [parse-symbol]]
            [chia.util.perf :as perf]
            [lark.tree.util :as util]
            [lark.tree.util :as util]
            #?@(:cljs
                [[cljs.tools.reader.reader-types :as r]
                 [cljs.tools.reader.edn :as edn]
                 [applied-science.js-interop :as j]])
            #?@(:clj
                [[clojure.tools.reader.reader-types :as r]
                 [clojure.tools.reader.edn :as edn]])
            [lark.tree.format :as format]))

(declare parse-next)

(defn read-to-char-boundary
  [reader]
  (let [c (r/read-char reader)]
    (str c
         (if (identical? c \\)
           ""
           (rd/read-until reader rd/boundary?)))))

(defn dispatch
  [c]
  (cond
    (or (identical? c \space)
        (identical? c rd/non-breaking-space)
        (identical? c \tab)) :space

    (identical? c (first rd/*delimiter*)) :matched-delimiter
    (identical? c \() :list
    (rd/newline? c) :newline
    (identical? c \[) :vector
    (identical? c \") :string
    (identical? c \^) :meta
    (identical? c \:) :keyword
    (identical? c \;) :comment
    (identical? c \{) :map
    (identical? c \#) :sharp
    (nil? c) :eof
    (identical? c \@) :deref
    (identical? c \,) :comma

    (perf/identical-in? [\}
                         \]
                         \)] c) :unmatched-delimiter
    (identical? c \') :quote
    (identical? c \~) :unquote
    (identical? c \`) :syntax-quote
    :else :token))

(defn printable-only? [n]
  (when n
    (or
     (perf/keyword-in? [:space :comma :newline :comment :comment-block]
                       (.-tag n))
     (get (.-options n) :invalid?))))

;; -------------- from cljs.tools.reader.edn ------------------

(defn- macro-terminating? [ch]
  (perf/identical-in? [")"
                       "]"
                       "}"
                       "{"
                       "\""
                       "["

                       ;; the chars below are never found?

                       "^"
                       "("
                       "\\"] ch))

(defn read-token* ^string [rdr initial-ch]
  (loop [out ""
         ch initial-ch]
    (cond (or (rd/whitespace? ch)
              (macro-terminating? ch)) (do (r/unread rdr ch) out)

          (nil? ch) out

          :else
          (recur (perf/str out ch) (r/read-char rdr)))))

;; -------------------------------------------------------------

(defn parse-token
  "Parse a single token."
  [reader]
  (let [ch (r/read-char reader)
        token (if (identical? ch "\\")
                (js* "~{} += ~{}" ch (read-to-char-boundary reader))
                (read-token* reader ch))
        tag (if (some-> token
                        (subs 0 1)
                        (js/parseInt)
                        (js/isNaN))
              :token
              :number)]
    (rd/ValueNode tag token)))

(defn take-printable-children
  [reader tag n]
  (rd/conj-children
   reader
    (rd/StartingNode reader tag)
    {:read-fn parse-next
     :count-pred (complement printable-only?)
     :take-n n}))

#_(defn take-n-children* [reader tag prefix n first-printable-child-tag]
  (rd/conj-children reader
    (rd/StartingNode reader tag)
    {:read-fn parse-next
     :count-pred (complement printable-only?)
     :take-n n}))

(defn take-n-children
  [reader tag prefix n first-printable-child-tag]
  (let [[line col] (rd/current-pos reader)
        offset (rd/current-offset reader)
        [valid? children after] (rd/take-children reader {:read-fn parse-next
                                                          :count-pred (complement printable-only?)
                                                          :take-n n})
        invalid? (or (not valid?)
                     (and first-printable-child-tag
                          (-> (first (filter (complement printable-only?) children))
                              (.-tag)
                              (not= first-printable-child-tag))))]
    (if invalid?
      (rd/Splice
       (rd/InvalidToken! tag prefix [line
                                     (- col (count prefix))
                                     line
                                     col
                                     (- offset (count prefix))
                                     offset])
       (into children after))
      (-> (rd/StartingNode reader tag)
          (rd/assoc-children! children)
          (cond-> (seq after)
                  (rd/Splice after))))))

(defn parse-keyword
  [reader]
  (let [ch (r/read-char reader)
        resolve-ns? (= \: (r/peek-char reader))
        _ (when resolve-ns?
            (r/read-char reader))
        token (read-token* reader ch)
        ;anonymous? (= token ":")
        expr (try (edn/read-string token)
                  (catch js/Error e
                    ::error))]
    (if (= expr ::error)
      (rd/InvalidToken! :keyword
                        (cond->> token
                                 resolve-ns? (str ":")))
      (rd/->Node :keyword
                 (when resolve-ns?
                   {:resolve-ns? resolve-ns?}) nil expr nil nil))))

(defn parse-sharp
  [reader]
  (r/read-char reader)
  (case (r/peek-char reader)
    nil (rd/InvalidToken! :reader-macro "#")
    "{" (take-n-children reader :set "#" 1 :map)
    "(" (take-n-children reader :fn "#" 1 :list)
    "\"" (take-n-children reader :regex "#" 1 :string)
    "^" (do (r/read-char reader)
            (take-n-children reader :reader-meta "#^" 1 nil))
    "'" (do (r/read-char reader)
            (take-n-children reader :var "#'" 1 :token))
    "_" (do (r/read-char reader)
            (take-n-children reader :uneval "#_" 1 nil))
    "?" (do
          (r/read-char reader)
          (let [ch (r/peek-char reader)]
            (if-let [tag (case ch
                           "(" :reader-conditional
                           "@" (do (r/read-char reader)
                                   :reader-conditional-splice)
                           nil)]
              (take-n-children reader tag (str "#?" (when (= ch "@" "@"))) 1 :list)
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
                (-> (take-n-children reader :namespaced-map prefix 1 :map)
                    (assoc :options options))))
    (take-n-children reader :data-literal "#" 1 :token)
    ;; TODO
    ;; namespaced maps
    #_(rd/InvalidToken! :reader-macro "#")
    #_(take-printable-children reader :reader-macro 2)))

(defn parse-unquote
  [reader]
  (r/read-char reader)
  (let [c (r/peek-char reader)]
    (if (identical? c \@)
      (do (r/read-char reader)
          (take-printable-children reader :unquote-splicing 1))
      (take-printable-children reader :unquote 1))))

(defn parse-next*
  [reader]
  (let [c (r/peek-char reader)
        tag (dispatch c)]
    (case tag

      :token (parse-token reader)

      :keyword (parse-keyword reader)

      :sharp (parse-sharp reader)

      :comment (do (rd/ignore reader)
                   (rd/ValueNode tag (rd/read-until reader
                                                    (fn [x]
                                                      (or (nil? x) (#{\newline \return} x))))))

      (:deref
       :quote
       :syntax-quote) (take-n-children reader tag (r/read-char reader) 1 nil)

      :unquote (parse-unquote reader)

      :newline (do (rd/ignore reader)
                   (rd/ValueNode tag (str "\n" (rd/read-while reader rd/space?))))

      :comma (rd/ValueNode tag (rd/read-while reader #(identical? % c)))

      :space (rd/ValueNode tag (rd/read-while reader rd/space?))
      (:list
       :vector
       :map) (rd/NodeWithChildren reader parse-next tag (emit/bracket-match c))

      :matched-delimiter (rd/ValueNode tag (r/read-char reader))

      :unmatched-delimiter (-> (rd/ValueNode tag (r/read-char reader))
                               (assoc :info {:direction :backward
                                             :expects (emit/bracket-match c)}))

      :eof nil

      :meta (do (r/read-char reader)
                (take-printable-children reader tag 1))

      :string (rd/read-string-data (rd/EmptyNode tag) reader))))

(defn parse-next
  [reader]
  (rd/read-with-position reader parse-next*))

(defn indexing-reader
  "Create reader for strings."
  [s]
  (r/indexing-push-back-reader
   (r/string-push-back-reader s 10)))

(defn ast*
  [s]
  (binding [rd/*invalid-nodes* (volatile! [])]
    (let [reader (indexing-reader s)
          base (rd/conj-children reader
                 (rd/EmptyNode :base)
                 {:read-fn parse-next})]
      (-> base
          (rd/assoc-range! [0 0
                            (dec (.-line reader))
                            (dec (.-column reader))
                            0 (.-length s)])
          (assoc :invalid-nodes (util/guard-> @rd/*invalid-nodes* seq))))))

(defn ast
  [s]
  (let [base (ast* s)]
    (-> base
        (assoc :string s
               :children (let [children (.-children base)
                               end (dec (count children))]
                           (when-not (neg? end)
                             (loop [i 0
                                    children (transient children)]
                               (if (identical? i end)
                                 (persistent! children)
                                 (let [node (nth children i)]
                                   (recur (inc i)
                                          (assoc! children i
                                                  (rd/assoc-string! node (subs s (:offset node) (:end-offset node))))))))))))))


