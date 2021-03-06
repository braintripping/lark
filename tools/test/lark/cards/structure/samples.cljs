(ns lark.cards.structure.samples
  (:require [lark.cards.structure.serialize-selections :as ss]
            [lark.structure.operation :as operation]
            [lark.tree.emit :as emit]
            [lark.tree.core :as tree]
            [chia.util :as u]
            [lark.structure.pointer :as pointer]
            [lark.fast-zip :as z]))

(defn with-args [args & pairs]
  (let [triples (partition 2 (interleave (partition 2 pairs) (if (vector? args) (repeat args) args)))]
    (for [[[in out] args] triples]
      [in {args out}])))

(def samples
  {#_#_:identity (->> ["^|js x"
                   "^j|s x"
                   "< [] >"
                   "#<{>}"
                   "<#>{}"
                   "abc |abc"
                   "abc| abc"
                   "[ab<c> def]"
                   "[ab<c d>ef]"
                   "[abc< d>ef]"
                   ]
                  (mapcat (fn [sample] [sample {[] sample}])))
   :edit/replace [(with-args ["Y"]
                    "< d>" "Y|"
                    "  |  " "  Y|  "


                    "a<b>c" "aY|c"
                    "a<b c>d" "aY|d"
                    "[a<b] [c>d]" "[aY|d]"
                    "[[a<b]]x[c>d]" "[[a]Y|d]"
                    "[[a<b]c]d>" "[[a]]Y|"
                    "<   > " "Y| "

                    "<a [b>c]" "[Y|c]"

                    "(<x>)" "(Y|)"
                    "[<()x>]" "[Y|]"
                    "[<x()>]" "[Y|]"
                    "<a []>" "Y|"
                    "<[]>" "Y|")

                  (with-args ["("]
                    "ab|cd" "ab(|cd")
                  (with-args [""]
                    "(< [] >)" "(|)"
                    "(<a>)" "(|)")
                  (with-args [" "]
                    "( x|)" "( x |)")

                  "a|" {["x"] "ax|"
                        ["\na"] "a\na|"
                        ["\n"] "a\n|"}
                  "|a" {["x"] "x|a"
                        ["\na"] "\na|a"}

                  "|js x" {["^"] "^|js x"}
                  " \"a|bc\" " {[""] " \"a|bc\" "
                                ["X"] " \"aX|bc\" "}

                  (for [s ["(" ")" "[" "]" "#" "^" "@" "~"]]
                    ["( | )" {[s] (str "( " s "| )")}])

                  (with-args ["x"]
                    "|a|" "x|ax|"
                    "[|]" "[x|]"
                    "|[]" "x|[]"
                    "[]|" "[]x|"
                    "|@a" "x|@a"
                    " \"| " " \"x| "

                    "(()|)" "(()x|)"                        ;; offset from right

                    "@|a" "@x|a"
                    "^j|s[]" "^jx|s[]"
                    "a<a>a" "ax|a"
                    "<a>a<a>" "x|ax|")]
   #_#_:selection/move [
                    (with-args (for [i (range 6)]
                                 [:x i])
                      "|([])" "|([])"
                      "|([])" "(|[])"
                      "|([])" "([|])"
                      "|([])" "([]|)"
                      "|([])" "([])|")
                    "a |\n| b" {[:x 1] "a \n| |b"
                                [:x -1] "a| |\n b"
                                [:y 1] "a \n b|"
                                [:y -1] "|a \n b"}
                    "(()|x)" {[:x 1] "(()x|)"
                              [:x -1] "((|)x)"
                              [:x -2] "(|()x)"}
                    ; cursors
                    "|" {[:x 1] "|"
                         [:x -1] "|"
                         [:y 1] "|"
                         [:y -1] "|"}
                    "| |" {[:x 1] " |"
                           [:x -1] "| "
                           [:y 1] " |"
                           [:y -1] "| "}
                    "|[ ] " {[:x 1] "[| ] "
                             [:x -1] "|[ ] "
                             [:y 1] "[ ] |"
                             [:y -1] "|[ ] "}               ;{:+ch " insert node"}
                    "[ ]|" {[:x 1] "[ ]|"
                            [:x -1] "[ |]"
                            [:y 1] "[ ]|"
                            [:y -1] "|[ ]"}                 ;{:+ch " insert node"}
                    "[|]" {[:x 1] "[]|"
                           [:x -1] "|[]"
                           [:y 1] "[]|"
                           [:y -1] "|[]"}                   ;{:+ch " insert node"}
                    "[| ] " {[:x 1] "[ |] "
                             [:x -1] "|[ ] "
                             [:y 1] "[ ] |"
                             [:y -1] "|[ ] "}               ;{:+ch " right+"}
                    "[ |] " {[:x 1] "[ ]| "
                             [:x -1] "[| ] "
                             [:y 1] "[ ] |"
                             [:y -1] "|[ ] "}               ;{:+ch " left+"}
                    "#|{}" {[:x 1] "#{|}"
                            [:x -1] "|#{}"
                            [:y 1] "#{}|"
                            [:y -1] "|#{}"}                 ;{:+ch " ? eg _, :kw, ::, @"}
                    "|abc " {[:x 1] "a|bc "
                             [:x -1] "|abc "
                             [:y 1] "abc |"
                             [:y -1] "|abc "}               ;{:+ch " +right"}
                    "a|bc " {[:x 1] "ab|c "
                             [:x -1] "|abc "
                             [:y 1] "abc |"
                             [:y -1] "|abc "}               ;{:+ch " +loc"}
                    "abc| " {[:x 1] "abc |"
                             [:x -1] "ab|c "
                             [:y 1] "abc |"
                             [:y -1] "|abc "}
                    "a<b>c" {[:y 1] "abc|"
                             [:y -1] "|abc"}
                    "|aa|aaa" {[:x 1] "a|aa|aa"}]})

(defn sample->state [sample-source]
  (let [{:keys [source
                spans]} (ss/read-coord-spans sample-source)
        zip (tree/string-zip source)
        selections (mapv #(pointer/pointer-span zip %) spans)]
    {:-source source
     :ast (z/node zip)
     :selections selections}))

(defn operate-on-sample [op-key op-args sample-before expected]
  (let [state-before (sample->state sample-before)
        {:as state-after
         :keys [error
                ast
                selections]} (operation/operate state-before op-key op-args)
        zip (tree/zip ast)
        sample-after (some-> ast
                             (emit/string)
                             (ss/write-pointer-spans (mapv #(pointer/resolve-span zip %) selections)))
        fail? (and (u/some-str expected)
                   (not= expected sample-after))]
    {:op-key op-key
     :op-args op-args
     :sample sample-before
     :sample-expected expected
     :sample-actual sample-after
     :source (:-source state-before)
     :state-before state-before
     :state-after (update state-after :error (fn [x]
                                               (or x
                                                   (when fail?
                                                     {:expected expected
                                                      :actual sample-after}))))}))
