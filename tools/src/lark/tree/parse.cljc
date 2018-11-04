;; modified from https://github.com/rundis/rewrite-cljs
;; https://github.com/rundis/rewrite-cljs/blob/master/LICENSE

(ns lark.tree.parse
  (:require [lark.tree.reader :as rd]
            [lark.tree.emit :as emit]
            [cljs.tools.reader.impl.commons :refer [parse-symbol]]
            [chia.util.perf :as perf]
            [lark.tree.util :as util]
            [lark.tree.util :as util]
            #?@(:cljs
                [[cljs.tools.reader.reader-types :as r]
                 [cljs.tools.reader.edn :as edn]
                 [chia.util.js-interop :as j]])
            #?@(:clj
                [[clojure.tools.reader.reader-types :as r]
                 [clojure.tools.reader.edn :as edn]])))

(declare parse-next)

(defn boundary? [x]
  (or (rd/whitespace? x)
      (rd/boundary? x)))

(defn read-to-char-boundary
  [reader]
  (let [c (r/read-char reader)]
    (str c
         (if (identical? c \\)
           ""
           (rd/read-until reader boundary?)))))

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

(defn take-printable-children
  [reader tag n]
  (rd/conj-children
   (rd/EmptyNode tag)
   reader
   {:read-fn parse-next
    :count-pred (complement printable-only?)
    :take-n n}))

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
    (rd/ValueNode tag token)
    ;; TODO
    ;; is it important to detect invalid tokens?
    #_(try (let [[tag value] (let [value (edn/read-string token)]
                               (if (symbol? value) [:symbol value]
                                                   [:token token]))]
             (prn [tag value])
             (rd/ValueNode tag value))
           (catch js/Error e
             (rd/report-invalid!
              (rd/->Node :token
                         {:info {:tag (or (some-> (re-find #"symbol|number" (ex-message e))
                                                  (keyword))
                                          :token)}}
                         nil
                         token
                         nil))))))

(defn take-n-children
  ([reader tag prefix n] (take-n-children reader tag prefix n nil))
  ([reader tag prefix n first-printable-child-tag]
   (let [[line col] (rd/current-pos reader)
         offset (rd/current-offset reader)
         [valid? children after] (rd/take-children reader {:read-fn parse-next
                                                           :count-pred (complement printable-only?)
                                                           :take-n n})]
     (if (and valid? (or (nil? first-printable-child-tag)
                         (-> (first (filter (complement printable-only?) children))
                             (.-tag)
                             (= first-printable-child-tag))))
       (-> (rd/EmptyNode tag)
           (assoc :children children)
           (cond-> (seq after)
                   (rd/Splice after)))
       (rd/Splice
        (rd/InvalidToken! tag prefix [line
                                      (- col (count prefix))
                                      line
                                      col
                                      (- offset (count prefix))
                                      offset])
        (into children after))))))

#_(defn try-take-all-children [reader tag]
    (let [[valid? children after :as result] (rd/take-children reader {:read-fn parse-next})]
      (if valid?
        (-> (rd/EmptyNode tag)
            (assoc :children children)
            (cond-> (seq after)
                    (rd/Splice after)))
        (into children after))))

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
                   {:resolve-ns? resolve-ns?}) nil expr nil))))

(defn parse-sharp
  [reader]
  (r/read-char reader)
  (case (r/peek-char reader)
    nil (rd/InvalidToken! :reader-macro "#")
    "{" (take-n-children reader :set "#" 1 :map)
    "(" (take-n-children reader :fn "#" 1 :list)
    "\"" (take-n-children reader :regex "#" 1 :string)
    "^" (do (r/read-char reader)
            (take-n-children reader :reader-meta "#^" 2))
    "'" (do (r/read-char reader)
            (take-n-children reader :var "#'" 1 :token))
    "_" (do (r/read-char reader)
            (take-n-children reader :uneval "#_" 1))
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
    ;; TODO
    ;; namespaced maps
    (rd/InvalidToken! :reader-macro "#")
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
       :syntax-quote) (do (r/read-char reader)
                          (take-n-children reader tag c 1))

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
                (take-printable-children reader tag 2))

      :string (rd/read-string-data (rd/EmptyNode tag) reader))))

(defn parse-next
  [reader]
  (rd/read-with-position reader parse-next*))

(defn indexing-reader
  "Create reader for strings."
  [s]
  (r/indexing-push-back-reader
   (r/string-push-back-reader s 10)))

(defn ast
  [s]
  (binding [rd/*invalid-nodes* (volatile! [])]
    (let [reader (indexing-reader s)
          base (rd/conj-children (rd/->Node :base nil nil nil nil)
                                 reader
                                 {:read-fn parse-next})
          base (rd/assoc-range! base [0 0
                                      (dec (.-line reader))
                                      (dec (.-column reader))
                                      0 (.-length s)])]
      (let [[source children]
            (reduce (fn [[source values] {:as node
                                          :keys [offset end-offset]}]
                      (let [node-str (subs s offset end-offset)]
                        [(str source node-str)
                         (conj values (assoc node :source node-str))]))
                    ["" []] (.-children base))]
        (assoc base
          :source source
          :children children
          :invalid-nodes (util/guard-> @rd/*invalid-nodes*
                                       (comp not empty?)))))))
