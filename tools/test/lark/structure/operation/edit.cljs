(ns lark.structure.operation.edit
  (:require [fast-zip.core :as z]
            [lark.tree.core :as tree]
            [lark.structure.delta :as delta]
            [lark.structure.operation.impl :as impl]
            [lark.structure.operation.impl.replace :as replace]
            [lark.structure.pointer :as pointer]))

(defn replace-span [s state *span]
  (let [loc (tree/zip (:ast state))
        loc (if (pointer/same-paths? @*span)
              (replace/replace-at-path! loc @*span s)
              (replace/replace-span! loc *span s))]
    (when-let [to (:to @*span)]
        (reset! *span {:from to}))
    (assoc state :ast (z/root loc))))

(defmethod impl/-operate :edit/replace
  [_ state [s]]
  (->> (delta/selections)
       (reduce (partial replace-span s) state)))

