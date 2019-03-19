(ns lark.cards.structure.serialize-selections
  (:require [lark.cards.structure.core :as structure]))


(defn in-selection! [selections]
  (assert (:open? (last selections))
          (str "Must be in selection: " selections)))

(defn not-in-selection! [selections]
  (assert (not (some-> (last selections) :open?))
          (str "Must not be in selection: " selections)))

(defn read-ranges [s]
  (loop [segments (cons (second (re-find #"([^\n|<>]*)" s))
                        (->> (re-seq #"([\n\|<>])([^\n\|<>]*)" s)
                             (mapcat #(subvec (vec %) 1))))
         source ""
         line 0
         column 0
         selections []]
    (if-not segments
      {:source source
       :ranges selections}
      (let [text (first segments)]
        (case text
          "|" (do (not-in-selection! selections)
                  (recur (next segments) source line column (conj selections {:line line :column column})))
          "<" (do (not-in-selection! selections)
                  (recur (next segments) source line column (conj selections {:line line :column column :open? true})))
          ">" (do (in-selection! selections)
                  (recur (next segments) source line column (update selections (dec (count selections)) assoc :end-line line :end-column column :open? false)))
          "\n" (recur (next segments) (str source text) (inc line) 0 selections)
          (recur (next segments) (str source text) line (+ column (count text)) selections))))))

(defn write-ranges [s ranges]
  (->> (reverse (sort-by (juxt :line :column :end-line :end-column) ranges))
       (reduce (fn [s {:keys [line
                              column
                              end-line
                              end-column]}]
                 (if end-line
                   (-> s
                       (structure/insert-at-coords [end-line end-column] ">")
                       (structure/insert-at-coords [line column] "<"))
                   (structure/insert-at-coords s [line column] "|"))) s)))


