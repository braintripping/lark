(ns lark.tree.util
  (:require [chia.util.macros :as m]
            [clojure.pprint :as pp]))

;; Successive `identical?` comparisons are _significantly_ faster than idiomatic alternatives such as `(contains? #{:k1 :k2} the-keyword)`,
;; results in a 2x overall speedup in parse/ast.

(defn some-str [s]
  (when (and (string? s) (not (identical? s "")))
    s))

(defn guard-> [x f]
  (when (f x) x))

(defn guard->> [f x]
  (when (f x) x))

#?(:cljs (defn log-current-stack []
           (try (throw (js/Error.))
                (catch js/Error e
                  (js/console.log (.-stack e))))))

#?(:cljs (do
           (def hist (volatile! {}))
           (defn hist-count! [label x true?]
             (when true? (vswap! hist update [label x] inc))
             true?)

           (defonce _
                    (let [last (atom nil)]
                      (js/setInterval #(when (not= @last @hist)
                                         (reset! last @hist)
                                         (->> @hist
                                              (seq)
                                              (sort-by (fn [[[label x] value]]
                                                         [label value]))
                                              (reverse)
                                              (pp/pprint)
                                              (with-out-str)
                                              (js/console.log))) 3000)))))