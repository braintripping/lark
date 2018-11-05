(ns lark.tree.format-test
  (:require [lark.tree.core :as tree]
            [cljs.test :refer [deftest is are testing]]
            [clojure.string :as str]
            [fast-zip.core :as z]
            [lark.tree.emit :as emit]
            [lark.tree.format :as format]
            [lark.tree.nav :as nav]
            [cljs.pprint :as pp]
            [lark.tree.parse :as parse]))

(deftest format
  (doall (for [[in out] (->> ["(-> {} \n (assoc :a 1\n :b 2))"
                              "(-> {} \n    (assoc :a 1\n           :b 2))"

                              "(-> {} \n (assoc \n :a 1))"
                              "(-> {} \n    (assoc \n      :a 1))"

                              "()[\n]" "()[\n   ]"
                              "[\n]   {\n}" "[\n ] {\n    }"
                              "[1\n2]"
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


                              "(a\n)"
                              "(a\n )"

                              "(def x
                               1)
                               x"
                              "(def x\n  1)\nx"]

                             (partition 2))]
           (is (= (tree/format-string in) out)))))

(extend-type z/ZipperLocation
  IDeref
  (-deref [x] (.-node x))
  IPrintWithWriter
  (-pr-writer [o writer _]
    (-write writer
            (str "#Z" (prn-str (z/node o))))))

(defn p [x] (doto x prn))

(defn traverse-zip [loc f]
  (loop [loc (z/next loc)]
    (f loc)
    (if (z/end? loc)
      nil
      (recur (z/next loc)))))

(defn range-accurate? [loc]
  (is (= (-> (.-node loc)
             :range
             (nth 3))
         (-> (emit/string loc)
             (parse/ast)
             :range
             (nth 3)))
      (.-node loc)))

(defn ranges-accurate? [loc]
  (traverse-zip loc range-accurate?))

#_(deftest update-ranges
  (doseq [s ["(a)"
             "( a)"
             "(a )"
             "1  2  3"
             ]]
    (let [z (tree/string-zip s)
          z2  (format/format-zip z)]
     #_ (ranges-accurate? z)
     (prn :start z (emit/string z))
     (prn :formatted z2 (emit/string z2))
      (ranges-accurate? z2)))
  #_(let [s "( a)"
        z (tree/string-zip s)
        z2 (tree/format-zip z)]
    (range-accurate? z)
    (range-accurate? z2)
    (pp/pprint {:before [@z (:range @z) (emit/string z)]
                :after [@z2 (:range @z2) (emit/string z2)]})
    )
  #_(-> "( )"
        (tree/string-zip)
        (p)
        (tree/format-zip)
        (p)))


