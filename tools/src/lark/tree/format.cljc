(ns lark.tree.format
  (:require [clojure.string :as str]
            [lark.tree.reader :as rd]
            [lark.tree.util :as util]
            [fast-zip.core :as z]))

(def INDENT " ")

(defn repeat-string [content n]
  (loop [i 0
         out ""]
    (if (= i n)
      out
      (recur (inc i) (str out content)))))

(def ^:dynamic *indent-level* 0)
(def ^:dynamic *prettify* false)

(defn indentation-for [x]
  (case x

    ("bound-fn" "extend" "extend-protocol" "extend-type" "fn" "ns" "reify")
    :indent

    ("cond" "do" "finally" "try" "with-out-str" "go" )
    0

    ("assoc" "apply" "binding" "case" "definterface" "defstruct" "deftype" "doseq" "dotimes" "doto"
     "for" "if" "if-let" "if-not" "if-some" "let" "letfn" "locking" "loop"
     "struct-map" "when" "when-first" "when-let" "when-not" "when-some"
     "while" "with-bindings" "with-local-vars" "with-open" "with-redefs"
     "with-redefs-fn" "go-loop" "are" "deftest" "testing")
    1

    ("catch" "condp" "proxy")
    2
    (cond (str/starts-with? x "def") :indent
          (re-find #"with|when|if" x) 1
          (str/ends-with? x "->") 0
          :else 0)))

(defn threading-node?
  [node]
  (when-let [operator (and (= (get node :tag) :list)
                           (some-> node
                                   (get :children)
                                   (first)))]
    (and (= :symbol (get operator :tag))
         (str/ends-with? (name (get operator :value)) "->"))))

(defn child-depth*
  ([node left-width] (child-depth* node left-width nil))
  ([{:keys [tag children column]} left-width {:keys [child threading?]}]
   (let [{:as operator
          op-tag :tag
          op-value :value} (first children)]
     (if (and (= :list tag)
              (= :symbol op-tag))
       (let [indent-type (indentation-for (name op-value))]
         (case indent-type
           :indent (+ column left-width 1)
           (let [indent-offset (if threading? (dec indent-type)
                                              indent-type)
                 items (sequence (comp (filter #(not= (get % :tag) :space))
                                       (take-while #(and (not= (get % :tag) :newline)
                                                         (or (nil? %) (not= % child))))
                                       (drop (inc indent-offset))) children)]
             (or (some-> (first items)
                         (:column))
                 (+ column left-width 1)))))
       (+ column left-width)))))

(defn indentation-parent? [node]
  (util/contains-identical-keyword? [:vector :list :map] (get node :tag)))

(defn child-indent-string [child-loc node]
  (let [child (z/node child-loc)
        left-edge-width (count (str (get-in node [:options :prefix])
                                    (first (get rd/edges (get node :tag)))))
        child-depth (child-depth* node left-edge-width {:child child
                                                        :threading? (some-> (z/up child-loc)
                                                                            (z/up)
                                                                            (z/node)
                                                                            (threading-node?))})]
    (repeat-string INDENT child-depth)))

(defn pad-chars?
  "Returns true if space should be left inbetween characters c1 and c2."
  [c1 c2]
  (prn c1 c2)
  (doto (cond (rd/close-bracket? c2) false
              (rd/open-bracket? c1) false
              :else true)
    (prn)))