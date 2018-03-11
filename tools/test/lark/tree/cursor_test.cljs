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
            [lark.tree.util :as util]))

(extend-type z/ZipperLocation
  IPrintWithWriter
  (-pr-writer [o writer _]
    (-write writer
            (prn-str [:L (tree/string (z/node o)) (z/node o)]))))


(defn prepare-editor [value]
  (-> (utils/editor)
      (doto (.setValue value))
      (utils/deserialize-selections!)
      (doto (cm/update-ast!)
            (cm/update-cursor! true))))

(defn path-sexp [loc path]
  (some-> (nav/get-loc loc path)
          (z/node)
          (tree/sexp)))

(defn sexp-loc [ast value]
  (->> (iterate z/next ast)
       (take-while #(and % (not (z/end? %))))
       (filter (comp (partial = value) tree/sexp z/node))
       (first)))

(deftest resolving-paths
  (binding [format/*prettify* true]
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
           :let [ast (tree/string-zip in-str)
                 ast2 (-> (emit/string ast)
                          (tree/string-zip))
                 sexp1 (path-sexp ast path)
                 sexp2 (path-sexp ast2 path)

                 resolved-loc (when sexp (sexp-loc ast2 sexp))
                 resolved-sexp (when sexp (tree/sexp (z/node resolved-loc)))
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
                   :in-sexp sexp}))))))


(doall (for [[in-str in-sexp in-sticky in-formatted]
             '[["(a| b)" a :outer-right "(a| b)"]
               ["a |\n" nil :outer-left "a |\n"]
               ["(|)" () :inner-left "(|)"]
               ["(|    )" () :inner-left "(|)"]
               ["(|\"a\")" "a" :outer-left "(|\"a\")"]
               ["sy|mbol" symbol :terminal-offset "sy|mbol"]
               ["( ( ( a b | ) ) )" (a b) :inner-right "(((a b|)))"]
               ["(a |     b)" b :outer-left "(a |b)"]
               ["( |   )" () :inner-right "(|)"]
               ["(a |)" (a) :inner-right "(a|)"]
               ["(a |  z)" z :outer-left "(a |z)"]
               ["+  |    []" [] :outer-left "+ |[]"]
               ["()    | a b" a :outer-left "() |a b"]
               ["(a\n   | )" (a) :inner-right "(a\n |)"]
               ["(-> {|})" {} :inner-left "(-> {|})"]
               ["(-> {}|)" {} :outer-right "(-> {}|)"]
               ["(-> |{})" {} :outer-left "(-> |{})"]
               ["(->| {})" -> :outer-right "(->| {})"]]
             :let [{:as editor
                    {:keys [pos]} :magic/cursor
                    :keys [zipper]} (prepare-editor in-str)

                   ;; resolve loc from editor cursor position
                   [path resolved-sticky :as cursor-path] (cursor/path zipper pos)
                   resolved-loc (nav/get-loc zipper path)
                   resolved-sexp (some-> resolved-loc z/node tree/sexp)

                   ;; format the code and put in a new editor
                   formatted-str (tree/format (.getValue editor))
                   formatted-editor (doto editor
                                      (.setValue formatted-str))
                   formatted-zipper (:zipper formatted-editor)

                   ;; resolve loc in formatted editor
                   formatted-resolved-loc (nav/get-loc formatted-zipper path)
                   formatted-resolved-sexp (some-> formatted-resolved-loc
                                                   (z/node)
                                                   (tree/sexp))

                   ;; putting cursor back into formatted editor
                   formatted-cursor (cursor/position formatted-zipper cursor-path)
                   _ (doto formatted-editor
                       (.setCursor (cm/range->Pos formatted-cursor))
                       (utils/serialize-selections!))
                   formatted-str-with-cursor (.getValue formatted-editor)
                   error-info (merge
                               (when (not= [resolved-sexp resolved-sticky] [in-sexp in-sticky])
                                 {:RESOLVED [resolved-sexp resolved-sticky]})
                               (when (not= formatted-str-with-cursor in-formatted)
                                 {:FORMATTED-str formatted-str})
                               (when (not= formatted-resolved-sexp in-sexp)
                                 {:FORMATTED-sexp formatted-resolved-sexp}))]
             :when (not (empty? error-info))]

         (merge
          error-info
          {:in-str in-str
           :in-str-formatted in-formatted
           :expected [in-sexp in-sticky]
           :path path})))

(comment
 (let [ast (tree/string-zip "(((a b)))")]
   (nav/get-loc ast [0 0 0]))

 (binding [format/*prettify* true]
   (emit/string (parse/ast "(a\n    )"))))