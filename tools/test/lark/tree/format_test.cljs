(ns lark.tree.format-test
  (:require [lark.tree.core :as tree]
            [cljs.test :refer [deftest is are testing]]
            [fast-zip.core :as z]
            [lark.tree.emit :as emit]
            [lark.tree.format :as format]
            [lark.tree.nav :as nav]
            [cljs.pprint :as pp]
            [lark.tree.parse :as parse]
            [lark.tree.reader :as rd]))


(deftest format
  (doall (for [[in out] (->> ["()()"
                              "() ()"

                              "()a"
                              "() a"

                              "a()"
                              "a ()"

                              "( )"
                              "()"

                              "(1 )"
                              "(1)"

                              "(\n   a)"
                              "(\n a)"

                              "(a    \n    {})"
                              "(a \n {})"

                              "(-> {} \n (assoc :a 1\n :b 2))"
                              "(-> {} \n    (assoc :a 1\n           :b 2))"

                              "(-> {} \n (assoc \n :a 1))"
                              "(-> {} \n    (assoc \n      :a 1))"

                              "()[\n]" "() [\n    ]"
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
           (let [formatted (tree/format-string in)]
             (when-not (= formatted out)
               (js/console.log in)
               (js/console.log formatted)
               (js/console.log out))
             (is (= formatted out))))))

#_(extend-type z/ZipperLocation
    IDeref
    (-deref [x] (.-node x))
    IPrintWithWriter
    (-pr-writer [o writer _]
      (-write writer
              (str "#Z" (prn-str (z/node o))))))

(defn p [x] (doto x prn))



(defn traverse-zip [f loc]
  (loop [loc (z/next loc)]
    (f loc)
    (if (z/end? loc)
      loc
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
            z2 (format/format-zip z)]
        #_(ranges-accurate? z)
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

(comment 'cljs.core
    (deftest ^:dev/always emit-ast
      (doseq [s [""]]
        (let [dirty-ast (time (parse/ast s))
              #_#_formatted-ast (time (emit/materialize dirty-ast {:format true}))]


          #_(is (= dirty-ast (emit/materialize dirty-ast))
                "AST is not modified")
          (is (= s (:string dirty-ast))
              "String is emitted correctly")

          #_(.profile js/console "new formatting")
          #_(simple-benchmark [] (emit/materialize dirty-ast) 5)
          #_(simple-benchmark [] (emit/materialize clean-ast) 5)
          #_(.profileEnd js/console)

          #_(binding [format/*pretty* true]
              #_(simple-benchmark [] (-> (parse/ast s)
                                         (emit/string-old)
                                         (tree/ast)) 5)
              (simple-benchmark [] (-> (parse/ast s)
                                       :string) 5)))
        (let [ast (parse/ast* s)]
          (.profile js/console "new formatting")
          (simple-benchmark [] (-> (emit/materialize ast {:format true})
                                   :string) 10)
          (.profileEnd js/console)))))

;; Known inconsistency: meta tags


