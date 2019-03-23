(ns lark.cards.structure.serialize-selections
  "For representing cursors, |, and selections, <>."
  (:require [lark.cards.structure.string :as string]
            [lark.tree.range :as range]))

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
         ranges []]
    (if-not segments
      {:source source
       :ranges ranges}
      (let [text (first segments)]
        (case text
          "|" (do (not-in-selection! ranges)
                  (recur (next segments) source line column (conj ranges {:line line
                                                                          :column column})))
          "<" (do (not-in-selection! ranges)
                  (recur (next segments) source line column (conj ranges {:line line :column column :open? true})))
          ">" (do (in-selection! ranges)
                  (recur (next segments) source line column (update ranges (dec (count ranges)) assoc :end-line line :end-column column :open? false)))
          "\n" (recur (next segments) (str source text) (inc line) 0 ranges)
          (recur (next segments) (str source text) line (+ column (count text)) ranges))))))

(defn write-ranges [s ranges]
  (->> (reverse (sort-by (juxt :line :column :end-line :end-column) ranges))
       (reduce (fn [s {:as sel
                       :keys [line
                              column
                              end-line
                              end-column]}]
                 (if (or (nil? end-line)
                         (and (= line end-line)
                              (= column end-column)))
                   (string/replace-at-coords s {:from [line column]} "|")
                   (-> s
                       (string/replace-at-coords {:from [end-line end-column]} ">")
                       (string/replace-at-coords {:from [line column]} "<")))) s)))

