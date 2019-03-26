(ns lark.structure.serialize-selections
  "For representing cursors, |, and selections, <>."
  (:require [lark.structure.string :as string]
            [lark.structure.coords :as coords]))

(defn in-selection! [selections]
  (assert (:open? (last selections))
          (str "Must be in selection: " selections)))

(defn not-in-selection! [selections]
  (assert (not (some-> (last selections) :open?))
          (str "Must not be in selection: " selections)))

(defn read-coord-spans [s]
  (loop [segments (cons (second (re-find #"([^\n|<>]*)" s))
                        (->> (re-seq #"([\n\|<>])([^\n\|<>]*)" s)
                             (mapcat #(subvec (vec %) 1))))
         source ""
         line 0
         column 0
         spans []]
    (if-not segments
      {:source source
       :spans spans}
      (let [text (first segments)]
        (case text
          "|" (do (not-in-selection! spans)
                  (recur (next segments) source line column (conj spans {:from [line column]})))
          "<" (do (not-in-selection! spans)
                  (recur (next segments) source line column (conj spans {:open? true
                                                                         :from [line column]})))
          ">" (do (in-selection! spans)
                  (recur (next segments) source line column (update spans (dec (count spans))
                                                                    (fn [span]
                                                                      (-> span
                                                                          (dissoc :open?)
                                                                          (assoc :to [line column]))))))
          "\n" (recur (next segments) (str source text) (inc line) 0 spans)
          (recur (next segments) (str source text) line (+ column (count text)) spans))))))

(defn write-pointer-spans [s spans]
  (->> (reverse (sort-by (juxt :from :to) spans))
       (reduce (fn [s {:as span
                       :keys [from to]}]
                 (if (coords/point? span)
                   (string/replace-at-coords s {:from from} "|")
                   (-> s
                       (string/replace-at-coords {:from to} ">")
                       (string/replace-at-coords {:from from} "<")))) s)))


