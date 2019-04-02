(ns lark.tree.impl.parse
  (:require [chia.util.perf :as perf]
            [lark.tree.reader :as rd]
            [clojure.tools.reader.reader-types :as r]
            [applied-science.js-interop :as j]))

(defn ^boolean macro-terminating? [ch]
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

(defn read-to-char-boundary
  [reader]
  (let [c (r/read-char reader)]
    (str c
         (if (identical? c \\)
           ""
           (rd/read-until reader rd/boundary?)))))

(defn printable-only? [n]
  (when n
    (or
     (perf/keyword-in? [:space :comma :newline :comment :comment-block]
                       (.-tag n))
     (get (.-options n) :invalid?))))

(defn read-printable-children
  [reader tag n parse-next]
  (rd/conj-children
   reader
   (rd/StartingNode reader tag)
   {:read-fn parse-next
    :count-pred (complement printable-only?)
    :take-n n}))

(defn read-n-children
  [reader tag prefix n first-printable-child-tag parse-next]
  (let [line (rd/line reader)
        col (rd/column reader)
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
          (#?@(:cljs [j/assoc! .-children]
               :clj  [assoc! :children]) children)
          (cond-> (seq after)
                  (rd/Splice after))))))