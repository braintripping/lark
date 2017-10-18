(ns lark.structure.edit-tests
  (:require [lark.structure.codemirror]
            [lark.structure.edit :as edit]
            [lark.structure.test-utils :as utils :refer [test-exec]]
            [cljs.test :refer-macros [deftest is are testing]]))

(defn prn-ret-cm [cm]
  (prn (.getValue cm))
  (.log js/console (count (.listSelections cm)))
  cm)


(deftest edit-commands

  (testing "round-trip selections"

    (doseq [val ["|a"
                 "|a|"
                 "a|b"
                 "<a>"
                 "a<b>c"
                 "a<bc>de<f>h"]]
      (is (= val (-> (utils/editor)
                     (doto (.setValue val))
                     (utils/deserialize-selections)
                     (utils/serialize-selections)
                     (.getValue)))))

    )

  (are [cmd source post-source]
    (= (test-exec cmd source) post-source)

    edit/kill! "(prn 1 |2 3)" "(prn 1 |)"
    edit/kill! "[1 2 |'a b c' 3 4]" "[1 2 |]"
    edit/kill! "[1 2 '|a b c']" "[1 2 '|']"

    edit/cut-form "|(+ 1)" "|"
    edit/cut-form "(|+ 1)" "(| 1)"

    #(edit/cursor-skip! % :left) "( )|" "|( )"
    #(edit/cursor-skip! % :left) "( |)" "(| )"
    #(edit/cursor-skip! % :left) "( a|)" "( |a)"
    #(edit/cursor-skip! % :left) "( ab|c)" "( |abc)"
    #(edit/cursor-skip! % :left) "((|))" "(|())"

    edit/comment-line "abc|\ndef" ";;abc\ndef|"
    edit/comment-line "abc\n|def" "abc\n;;def|"
    edit/comment-line "abc|" ";;abc|"

    edit/uneval! "|[]" "|#_[]"
    edit/uneval! "[]|" "#_[]|"
    edit/uneval! "a|bcd" "#_abcd|"

    edit/unwrap! "[|]" " | "
    edit/unwrap! "[| ]" " |  "
    edit/unwrap! " [|]" "  | "
    edit/unwrap! "[ |]" "  | "
    edit/unwrap! "[ \"abc|\" ]" "[  abc|  ]"

    edit/expand-selection "(|a)" "(<a>)"
    edit/expand-selection "(a|b)" "(<ab>)"
    edit/expand-selection "(<a > )" "(<a  >)"
    edit/expand-selection "(<a b>)" "<(a b)>"

    #(edit/expand-selection-x % :left) "(<a>)" "<(a)>"
    #(edit/expand-selection-x % :left) "(b <a>)" "(<b a>)"
    #(edit/expand-selection-x % :left) "c (<b a>)" "c <(b a)>"
    #(edit/expand-selection-x % :left) "c <(b a)>" "<c (b a)>"

    edit/slurp-forward "(|) a" "(| a)"
    edit/slurp-forward "`(|) a" "`(| a)"
    edit/slurp-forward "@(|) a" "@(| a)"
    edit/slurp-forward "`@(|) a" "`@(| a)"
    edit/slurp-forward "~@(|) a" "~@(| a)"
    edit/slurp-forward "~@[|]a" "~@[|a]"

    ))

(comment edit/uneval-top-level-form "[[|]]" "#_[[|]]"
         edit/uneval-top-level-form "[\n[|]]" "#_[\n[|]]")
