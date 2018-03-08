(ns lark.tree.core
  (:refer-clojure :exclude [range])
  (:require [lark.tree.parse :as parse]
            [lark.tree.emit :as emit]
            [lark.tree.node :as n]
            [lark.tree.nav :as nav]
            [lark.tree.range :as range]
            [fast-zip.core :as z]))

;; Parse

(def ast
  "Given ClojureScript source, returns AST"
  parse/ast)

(def ast-zip n/ast-zip)

(def string-zip
  "Given ClojureScript source, returns zipper"
  (comp ast-zip parse/ast))

;; Emit

(def string emit/string)
(def sexp emit/sexp)
(defn edges [node] (get emit/edges (:tag node)))

;; Nodes

(def comment? n/comment?)

(def whitespace? n/whitespace?)
(def newline? n/newline?)
(def sexp? n/sexp?)
(def may-contain-children? n/may-contain-children?)
(def terminal-node? n/terminal-node?)
(def has-edges? n/has-edges?)

;; Navigation

(def child-locs nav/child-locs)
(def right-locs nav/right-locs)
(def left-locs nav/left-locs)
(def top-loc nav/top-loc)
(def closest nav/closest)
(def include-prefix-parents nav/include-prefix-parents)

(def navigate nav/navigate)
(def mouse-eval-region nav/mouse-eval-region)

;; Ranges

(def within? range/within?)
(def inside? range/inside?)
(def edge-ranges range/edge-ranges)
(def inner-range range/inner-range)
(def bounds range/bounds)
(def start->end range/start->end)
(def end->start range/end->start)

(def empty-range? range/empty-range?)
(def node-highlights range/node-highlights)

(comment

 (def log (atom []))

 (assert (range/within? {:line 1 :column 1
                         :end-line 1 :end-column 2}
                        {:line 1 :column 1}))

 (doseq [[sample-str [line column] expected-sexp expected-string] [["1" [0 0] 1 "1"]
                                                                   ["[1]" [0 0] [1] "[1]"]
                                                                   #_["#{}" [0 1] #{} "#{}"]
                                                                   #_["\"\"" [0 1] "" "\"\""]
                                                                   #_["(+ 1)" [0 0] nil nil]
                                                                   #_["(+ 1)" [0 1] '(+ 1) "(+ 1)"]
                                                                   #_["(+ 1)" [0 2] '+ "+"]
                                                                   #_["(+ 1)" [0 3] nil " "]
                                                                   #_["(+ 1)" [0 4] 1 "1"]
                                                                   #_["(+ 1)" [0 5] '(+ 1) "(+ 1)"]
                                                                   #_["(+ 1)" [0 6] nil nil]
                                                                   #_["\n1" [1 1] 1 "1"]]]
   ;(reset! log [])
   (let [result-node (-> (ast sample-str)
                         (navigate {:line line
                                    :column column}))]
     (prn :res result-node)
     (when-not (= (sexp result-node) expected-sexp)
       (prn :SEXP_DIFF {:str sample-str
                        :pos [line column]
                        :expected-sexp expected-sexp
                        :actual-sexp (sexp result-node)}))
     (when-not (= (string result-node) expected-string)
       (prn :STR_DIFF {:str sample-str
                       :pos [line column]
                       :expected-string expected-string
                       :actual-string (string result-node)})))))

#_(let [sample-code-string ""]
    (let [_ (.profile js/console "parse-ast")
          ast (time (parse/ast sample-code-string))
          _ (.profileEnd js/console)]
      (println :cljs-core-string-verify (= (emit/string ast) sample-code-string))))

(def shape parse/shape)
(def group-comment-blocks parse/group-comment-blocks)

(ast "#{")

(binding [emit/*features* #{:cljs}]
  (for [[in-string out] (->> '[[" "
                                "\n"
                                \tab
                                ",,\t\n"
                                "#_{}"
                                "; this is a comment\n"
                                ";; this is a comment\n"
                                "; this is a comment"
                                ";; this is a comment"
                                ";"
                                ";;"
                                ";\n"
                                ";;\n"] _
                               "4" 4
                               "sym" sym
                               "\"a\"" "a"
                               "'[" ::emit/INVALID_TOKEN
                               ["]"
                                "["
                                "^"
                                "#"
                                "#("
                                "#{"
                                "'"
                                "#{[]"] ::emit/INVALID_TOKEN
                               "3" 3
                               "\n" _
                               "[]" []
                               "()" ()
                               "@1" (deref 1)
                               "#(+)" (fn* [] (+))
                               "@()" (deref ())
                               "#{1}" #{1}
                               "#'wha" (var wha)
                               "~1" (clojure.core/unquote 1)
                               ["'1"
                                "`1"] (quote 1)
                               "#?(:clj 1 :cljs (+ 2))" [(+ 2)]
                               "^:yes {}" {}
                               "^{:no false} {}" {}
                               "::a/b" :a/b
                               ";a" _
                               "#_()" _
                               "(1)" (1)
                               "[1]" [1]
                               ["{1 2}"
                                "{1    2}"] {1 2}

                               ["@sym"
                                "@  sym"] (deref sym)
                               ["'sym"
                                "' sym"
                                "`sym"
                                "`  sym"] (quote sym)
                               ["~sym"
                                "~  sym"] (clojure.core/unquote sym)
                               "~@sym" (clojure.core/unquote-splicing sym)

                               "#'sym" (var sym)
                               "#'\nsym" (var sym)

                               "^:wha" ::emit/INVALID_TOKEN

                               "#(+ 1 1)" #(+ 1 1)

                               ["#^:wha {}"
                                "#^{:wha true} {}"] {}

                               ;; regexp's are never equal
                               #_["#\"[A-B]\"" [#"[A-B]"]]]
                             (apply hash-map)
                             (reduce-kv (fn [m s v]
                                          (if (vector? s)
                                            (reduce (fn [m s]
                                                      (assoc m s v)) m s)
                                            (assoc m s v))) {}))
        :let [expected-sexp (if (= '_ out) nil out)]]
    (let [the-ast (ast in-string)
          the-sexp (-> the-ast
                       (sexp)
                       (first))
          the-str (string the-ast)]
      (cond (not= the-sexp expected-sexp)
            [false :incorrect-sexp (sexp the-ast) the-ast :expected expected-sexp]
            (not= the-str in-string)
            [false the-str the-ast :expected in-string]
            :else
            true))))


(ast "#{[]")