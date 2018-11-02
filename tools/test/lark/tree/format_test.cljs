(ns lark.tree.format-test
  (:require [lark.tree.core :as tree]
            [cljs.test :refer [deftest is are testing]]
            [lark.tree.parse :as parse]
            [chia.util.js-interop :as j]))


(deftest format
         (doall (for [[in out] (->> ["[1\n2]"
                                     "[1\n 2]"


                                     "(+\n 1 ;hello \n 2)"
                                     "(+\n 1 ;hello \n 2)"

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
                                     "(-> {} \n    (assoc \n      :a 1))"

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
                                     "(a\n )"]
                                    (partition 2))]
                  (is (= (tree/format in) out)))))

