(ns lark.tree.cursor-test
  (:require [lark.structure.test-utils :as utils]
            [lark.editors.codemirror :as cm]
            [lark.tree.core :as tree]
            [fast-zip.core :as z]
            [lark.tree.cursor :as cursor]
            [cljs.test :refer [deftest is are]]
            [lark.tree.nav :as nav]
            [lark.tree.parse :as parse]
            [lark.tree.format :as format]
            [lark.tree.emit :as emit]
            [lark.tree.util :as util]
            [cljs.pprint :as pp]
            [lark.tree.range :as range]))

(extend-type z/ZipperLocation
  IPrintWithWriter
  (-pr-writer [o writer _]
    (-write writer
            (str "#Z<" (emit/string (z/node o)) (z/node o) ">"))))


(defn prepare-editor [value]
  (-> (utils/editor)
      (doto (.setValue value))
      (utils/deserialize-selections!)
      (doto (cm/update-ast!)
            (cm/update-cursor! true))))

(defn path-sexp [loc path]
  (some-> (nav/get-loc loc path)
          (z/node)
          (emit/sexp)))

(defn sexp-loc [ast value]
  (->> (iterate z/next ast)
       (take-while #(and % (not (z/end? %))))
       (filter (comp (partial = value) emit/sexp z/node))
       (first)))

(deftest resolving-paths
  (doall
   (for [[in-str path sexp] '[["(a b)" [0] (a b)]
                              ["(a b)" [0 0] a]
                              ["(a b)" [0 1] b]
                              ["(a b)" [0 2] nil]
                              ["( a b )" [0 0] a]
                              ["( a  b)" [0 1] b]
                              ["( ( a))" [0 0 0] a]
                              [" \n a" [1] a]
                              ["(a\n b)" [0 2] b]]
         :let [zip (tree/string-zip in-str)
               ast2 (tree/format-zip zip)
               sexp1 (path-sexp zip path)
               sexp2 (path-sexp ast2 path)

               resolved-loc (when sexp (sexp-loc ast2 sexp))
               resolved-sexp (when sexp (emit/sexp (z/node resolved-loc)))
               resolved-path (if sexp
                               (nav/get-path resolved-loc)
                               path)

               error-info (merge (when (not= sexp sexp1 sexp2)
                                   {:SEXP-ast1 sexp1
                                    :SEXP-ast2 sexp2})
                                 (when (not= sexp resolved-sexp)
                                   {::RESOLVE-sexp resolved-sexp})
                                 (when (not= resolved-path path)
                                   {:RESOLVE-loc resolved-loc
                                    :RESOLVE-path resolved-path}))]
         #_:when #_(not (empty? error-info))
         ]
     (is (empty? error-info)
         (merge error-info
                {:in-str in-str
                 :in-path path
                 :in-sexp sexp})))))

(deftest formatting-with-cursors
  (doall (for [[in-str expected-sexp expected-sticky expected-str-with-cursor]
               '[["( ( ( a b | ) ) )" (a b) :inner-right "(((a b|)))"]
                 ["(|    )" () :inner-left "(|)"]
                 ["(a |)" (a) :inner-right "(a|)"]
                 ["(a\n   | )" (a) :inner-right "(a\n |)"]
                 ["(a| b)" a :outer-right "(a| b)"]
                 ["a |\n" nil :outer-left "a |\n"]
                 ["(|)" () :inner-left "(|)"]
                 ["(|\"a\")" "a" :outer-left "(|\"a\")"]
                 ["sy|mbol" symbol :terminal-offset "sy|mbol"]
                 ["(a |     b)" b :outer-left "(a |b)"]
                 ["( |   )" () :inner-right "(|)"]
                 ["(a |  z)" z :outer-left "(a |z)"]
                 ["+  |    []" [] :outer-left "+ |[]"]
                 ["()    | a b" a :outer-left "() |a b"]
                 ["(-> {|})" {} :inner-left "(-> {|})"]
                 ["(-> {}|)" {} :outer-right "(-> {}|)"]
                 ["(-> |{})" {} :outer-left "(-> |{})"]
                 ["(->| {})" -> :outer-right "(->| {})"]]
               :let [{:as editor
                      {:keys [pos]} :magic/cursor
                      :keys [zipper]} (prepare-editor in-str)

                     ;; resolve loc from editor cursor position
                     [target-path target-sticky :as cursor-path] (cursor/path zipper pos)
                     target-loc (nav/get-loc zipper target-path)
                     target-sexp (some-> target-loc z/node emit/sexp)
                     #_#__ (pp/pprint {:original-node (z/node target-loc)
                                   :original-range (range/bounds (z/node target-loc))})

                     ;; format the code and put in a new editor

                     formatted-zipper (-> (.getValue editor)
                                          (tree/string-zip)
                                          (tree/format-zip))
                     formatted-str (-> formatted-zipper
                                       .-node
                                       :string)
                     formatted-editor (doto editor
                                        (.setValue formatted-str))

                     ;; resolve loc in formatted zipper
                     found-loc (nav/get-loc formatted-zipper target-path)
                     found-sexp (some-> found-loc
                                        (z/node)
                                        (emit/sexp))

                     ;; putting cursor back into formatted editor
                     found-cursor-position (cursor/position formatted-zipper cursor-path) #_(try (cursor/position formatted-zipper cursor-path)
                                                                                                 (catch js/Error e
                                                                                                   (prn :NO_WORK
                                                                                                        {:cursor-path cursor-path
                                                                                                         :in-str in-str
                                                                                                         :formatted-str formatted-str
                                                                                                         :f-resolved-loc found-loc})))
                     _ (doto formatted-editor
                         (.setCursor (cm/range->Pos found-cursor-position))
                         (utils/serialize-selections!))
                     formatted-str-with-cursor (.getValue formatted-editor)
                     error-info (merge
                                 (when (not= [target-sexp target-sticky] [expected-sexp expected-sticky])
                                   {:error/RESOLVED {:expected [expected-sexp expected-sticky]
                                                     :resolved [target-sexp target-sticky]}})
                                 (when (not= formatted-str-with-cursor expected-str-with-cursor)
                                   {:error/formatted-str-with-cursor formatted-str-with-cursor})
                                 (when (not= found-sexp expected-sexp)
                                   {:error/FORMATTED-sexp found-sexp}))]
               :when (not (empty? error-info))]

           (do
             (pp/pprint (merge
                         error-info
                         {:input/str in-str
                          :found/sexp found-sexp
                          :found/cursor-pos found-cursor-position
                          :expected/sexp expected-sexp
                          :expected/sticky expected-sticky
                          :expected/str expected-str-with-cursor
                          :path target-path}))
             (is (= [target-sexp target-sticky] [expected-sexp expected-sticky]))
             (is (= formatted-str-with-cursor expected-str-with-cursor))
             (is (= found-sexp expected-sexp))))))

(comment
 (let [ast (tree/string-zip "(((a b)))")]
   (nav/get-loc ast [0 0 0])))