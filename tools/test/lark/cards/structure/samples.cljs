(ns lark.cards.structure.samples
  (:require [lark.cards.structure.serialize-selections :as ss]
            [lark.cards.structure.core :as structure]
            [lark.tree.emit :as emit]
            [lark.tree.core :as tree]
            [chia.util :as u]))

(defn with-args [args & pairs]
  (for [[in out] (partition 2 pairs)]
    [in {args out}]))

(def samples
  {:identity (->> ["^|js x"
                   "^j|s x"
                   "< [] >"
                   "#<{>}"
                   "<#>{}"
                   "abc |abc"
                   "abc| abc"
                   "[ab<c> def]"
                   "[ab<c d>ef]"
                   "[abc< d>ef]"]
                  (mapcat (fn [sample] [sample {[] sample}])))
   :edit/insert-text ["a|" {["x"] "ax|"
                            ["\na"] "a\na|"
                            ["\n"] "a\n|"}
                      "|a" {["x"] "x|a"
                            ["\na"] "\na|a"}

                      "|js x" {["^"] "^|js x"}

                      (for [s ["(" ")" "[" "]" "#" "^" "@" "~"]]
                        ["( | )" {[s] (str "( " s "| )")}])

                      (with-args ["x"]
                        "|a|" "x|ax|"
                        "[|]" "[x|]"
                        "|[]" "x|[]"
                        "[]|" "[]x|"
                        "|@a" "x|@a"
                        " \"| " " \"x| "

                        "(()|)" "(()x|)"                    ;; offset from right

                        "@|a" "@x|a"
                        "^j|s[]" "^jx|s[]"
                        "a<a>a" "ax|a"
                        "<a>a<a>" "x|ax|"
                        "<[]>" "x|")

                      ]
   :cursor/move ["a |\n| b" {[:x 1] "a \n| |b"
                             [:x -1] "a| |\n b"
                             [:y 1] "a \n b|"
                             [:y -1] "|a \n b"}
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
                          [:y -1] "|[ ] "}                  ;{:+ch " insert node"}
                 "[ ]|" {[:x 1] "[ ]|"
                         [:x -1] "[ |]"
                         [:y 1] "[ ]|"
                         [:y -1] "|[ ]"}                    ;{:+ch " insert node"}
                 "[|]" {[:x 1] "[]|"
                        [:x -1] "|[]"
                        [:y 1] "[]|"
                        [:y -1] "|[]"}                      ;{:+ch " insert node"}
                 "[| ] " {[:x 1] "[ |] "
                          [:x -1] "|[ ] "
                          [:y 1] "[ ] |"
                          [:y -1] "|[ ] "}                  ;{:+ch " right+"}
                 "[ |] " {[:x 1] "[ ]| "
                          [:x -1] "[| ] "
                          [:y 1] "[ ] |"
                          [:y -1] "|[ ] "}                  ;{:+ch " left+"}
                 "#|{}" {[:x 1] "#{|}"
                         [:x -1] "|#{}"
                         [:y 1] "#{}|"
                         [:y -1] "|#{}"}                    ;{:+ch " ? eg _, :kw, ::, @"}
                 "|abc " {[:x 1] "a|bc "
                          [:x -1] "|abc "
                          [:y 1] "abc |"
                          [:y -1] "|abc "}                  ;{:+ch " +right"}
                 "a|bc " {[:x 1] "ab|c "
                          [:x -1] "|abc "
                          [:y 1] "abc |"
                          [:y -1] "|abc "}                  ;{:+ch " +loc"}
                 "abc| " {[:x 1] "abc |"
                          [:x -1] "ab|c "
                          [:y 1] "abc |"
                          [:y -1] "|abc "}
                 "a<b>c" {[:y 1] "abc|"
                          [:y -1] "|abc"}
                 "|aa|aaa" {[:x 1] "a|aa|aa"}]})

(defn sample->state [sample-source]
  (let [{:keys [source
                ranges]} (ss/read-ranges sample-source)
        ast (tree/ast source)
        selections (->> ranges
                        (mapv (partial structure/range->selection (tree/ast-zip ast))))]
    {:-source source
     :ast ast
     :selections selections}))

(defn operate-on-sample [op-key op-args sample-before expected]
  (let [state-before (sample->state sample-before)
        {:as state-after
         :keys [error
                ast
                selections]} (try (structure/operate op-key state-before op-args)
                                  (catch js/Error e
                                    (js/console.error sample-before e)
                                    {:error e}))
        sample-after (-> (emit/string ast)
                         (ss/write-ranges (mapv (partial structure/selection->range (tree/ast-zip ast)) selections)))
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





;; a selection is [from-path, to-path]
;; a range is {… :line, :column, :end-line, :end-column}