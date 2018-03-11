(ns lark.tree.parse-test
  (:require [lark.tree.parse :as parse]
            [lark.tree.core :as tree]
            [fast-zip.core :as z]
            [lark.tree.emit :as emit]
            [lark.tree.format :as format]
            [cljs.test :refer [deftest is are testing]]
            [lark.tree.reader :as rd]))

(def shape tree/shape)

(deftest parse
  (binding [lark.tree.emit/*ns* (symbol "lark.tree.parse-test")]
    (testing "parse and emit"

      (are [string res-sexp]
        (let [tree (parse/ast string)]
          (when res-sexp
            (is (= res-sexp (emit/sexp tree))
                (str "Correct sexp for: " (subs string 0 30))))
          (is (= string (emit/string tree)))
          (str "Correct emitted string for: " string))

        "1" '[1]
        "prn" '[prn]
        "\"hello\"" '["hello"]
        "" '[]
        ":hello" '[:hello]
        ":a/b" '[:a/b]
        "::wha" '[:lark.tree.parse-test/wha]
        "#(+)" '[#(+)]
        "[1 2 3]\n3 4  5, 9" '[[1 2 3] 3 4 5 9]
        "^:dynamic *thing*" '[^:dynamic *thing*]
        "(f x)" '[(f x)]
        "#{1}" '[#{1}]
        "#^:a {}" '[#^:a {}]
        "#'a" '[#'a]
        "@a" '[(deref a)]
        "#_()" '[#_()]
        "'a" '[(quote a)]
        "'a" '['a]
        "`a" '['a]
        "~a" '[~a]
        "#?(:cljs (+ 1 1))" '[(+ 1 1)]
        "#?@(:cljs (+ 1 1))" '[+ 1 1]
        "#?(:cljs 1 :cljs 2)" '[1]                          ;; only keep first match. probably should throw error for duplicate feature.
        "#?(:clj 1 :cljs 2)" '[2]
        "#?(:clj 1)" '[]
        "my:symbol" '[my:symbol])



      (let [regexp-string "#\"[a-z]\""
            tree (parse/ast "#\"[a-z]\"")]
        (is (regexp? (-> tree
                         (emit/sexp)
                         (first)))
            "Regular expression is returned from regex string. (Can't test equality, regex's are never equal.)")
        (is (= regexp-string (emit/string tree))
            "Regexp returns same string"))

      )

    (are [in-string the-shape]
      (is (= (shape (-> (parse/ast in-string)
                        (parse/group-comment-blocks))) the-shape))

      "\n" [:comment-block]
      "\n\n;A" [:comment-block]
      ";A" [:comment-block]
      " ;A" [[:space] :comment-block]
      "; a

      ; b" [:comment-block]
      "a/" [[:token]])


    (testing "always re-emits the same string"
      (are [in-string]
        (is (= in-string (binding [format/*prettify* true]
                           (emit/string (parse/ast in-string)))))
        ";; A"
        ";;; A"
        ";;;A"
        ";;  A"
        ";  AB"
        ";A\n1\n2\n3\n4\n5"

        ";A\n;2\n3\n4"

        "\n;A"
        "\n;A\n"

        ";; A\n;; \n;; B"

        ";; A \n\n\n;; B\n;; C"
        ";; # Hi!\n;; This"
        "("
        ")"
        "#{([(^.\" # ^ # . !@#$%6 65465436542150- < >>< <  ~!~ !@ ~ ' * % $"))




    (testing "selections"
      (binding [emit/*print-selections* true]
        (let [ast (parse/ast "(+ 1 2 3)")
              root (tree/ast-zip ast)
              root-string (comp emit/string z/root)
              cursor {:tag :cursor}
              select-rights (fn [loc n]
                              (let [[contents num] (loop [rights (z/rights loc)
                                                          out [(z/node loc)]
                                                          i 1]
                                                     (if (= i n)
                                                       [out i]
                                                       (recur (rest rights)
                                                              (conj out (first rights))
                                                              (inc i))))
                                    loc (z/replace loc (assoc (rd/EmptyNode :selection)
                                                         :children contents))]
                                (last (take num (iterate (comp z/remove z/right) loc)))
                                ))
              grow-selection-right (fn [loc]
                                     (let [node (-> loc z/right z/node)]
                                       (-> loc
                                           (z/edit update :value conj node)
                                           z/right
                                           z/remove
                                           z/up)))]

          (are [x y] (= x y)

                     (-> (z/down root)
                         (z/insert-right cursor)
                         (root-string)) "(+ 1 2 3)|"

                     (-> (z/down root)
                         (z/down)
                         (z/insert-right cursor)
                         (root-string)) "(+| 1 2 3)"

                     (-> (z/down root)
                         z/down
                         (select-rights 2)
                         (root-string)) "(‹+ ›1 2 3)"

                     #_(-> root
                         z/down
                         z/down
                         (z/insert-left {:tag :selection
                                         :value []})
                         z/left
                         (grow-selection-right)
                         (grow-selection-right)
                         (grow-selection-right)
                         root-string) #_"(‹+ 1› 2 3)"

                     ;;;; next
                     ;; - select by row/col
                     ;; - better selection navigation. eg/ provide

                     ))))))

(deftest prettify
  (binding [format/*prettify* true]
    (are [in out]
      (= (-> in
             parse/ast
             emit/string)
         out)

      "[1\n2]"
      "[1\n 2]"

      "(+\n1\n2)"
      "(+\n 1\n 2)"

      "(+        1\n2)"
      "(+ 1\n   2)"

      "(let [x 2] 3\n4)"
      "(let [x 2] 3\n           4)"

      "(let [x 2]\n4)"
      "(let [x 2]\n  4)"

      "[ {:a 1\n:b 2}]"
      "[{:a 1\n  :b 2}]"

      "(->  a\nb)"
      "(-> a\n    b)"

      "(->    \n    {})"
      "(-> \n {})"

      "(-> {} \n (assoc :a 1\n :b 2))"
      "(-> {} \n    (assoc :a 1\n           :b 2))"

      "(-> {} \n (assoc \n :a 1))"
      "(-> {} \n    (assoc \n     :a 1))"

      "(assoc {} \n :a 1)"
      "(assoc {} \n  :a 1)"

      "(assoc {} :a 1\n :b 2)"
      "(assoc {} :a 1\n          :b 2)"

      "(assoc {}\n :a 1\n :b 2)"
      "(assoc {}\n  :a 1\n  :b 2)"

      "(+\n  1\n  2)"
      "(+\n 1\n 2)"

      "(+ 1 \n 2)"
      "(+ 1 \n   2)"

      "(a b c d e\n )"
      "(a b c d e\n   )"

      "(a b \n c)"
      "(a b \n   c)"

      "(1 2 \n 3)"
      "(1 2 \n 3)"

      "(do 1 2\n 3)"
      "(do 1 2\n    3)"

      "(let [x 1] 2\n 3)"
      "(let [x 1] 2\n           3)"

      "(let [x 1]\n 2\n 3)"
      "(let [x 1]\n  2\n  3)"

      "(let\n [x 1]\n 2\n 3)"
      "(let\n [x 1]\n 2\n 3)"

      "()[\n]" "()[\n   ]"
      "[\n]   {\n}" "[\n ] {\n    }"

      "(a\n)"
      "(a\n )")))


(deftest invalid-forms
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
      (let [the-ast (tree/ast in-string)
            the-sexp (-> the-ast
                         (tree/sexp)
                         (first))
            the-str (tree/string the-ast)]
        (is (= the-sexp expected-sexp)
            "emit/sexp")
        (is (= the-str in-string)
            "emit/string")))))