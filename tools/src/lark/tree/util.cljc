(ns lark.tree.util
  (:require [chia.util.macros :as m]))

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