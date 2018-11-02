(ns lark.structure.test-utils
  (:require lark.editors.codemirror
            ["codemirror" :as CM]
            ["codemirror/addon/search/searchcursor" :as search-cursor]
            [clojure.string :as str]))


(def editor
  (memoize (fn ^js  []
             (CM (doto (.createElement js/document "div")
                   (->> (.appendChild js/document.body))) (clj->js {:mode "clojure"
                                                                    :magicBrackets true})))))

(defn regex-replace [^js cm pattern replace-f]
  (let [search-cursor (.getSearchCursor cm pattern (CM/Pos 0 0) true)]
    (loop [results []]
      (if (not (.findNext search-cursor))
        (when-not (empty? results)
          (do
            (.setSelections cm (clj->js results))
            (.replaceSelections cm (clj->js (mapv replace-f results)) "around")
            results))
        (recur (conj results (let [anchor (.from search-cursor)
                                   head (.to search-cursor)]
                               {:anchor anchor
                                :head head
                                :text (.getRange cm anchor head)})))))))

(defn replace-selections [^js cm f]
  (.replaceSelections cm (clj->js (mapv (fn [sel]
                                          (f {:anchor (.-anchor sel)
                                              :head (.-head sel)
                                              :text (.getRange cm (.-anchor sel) (.-head sel))})) (.listSelections cm))) "around"))

(defn deserialize-selections!
  "Turn <ranges> into selected ranges."
  [^js cm]
  (regex-replace cm #"(<[^>]*>)|\|" (fn [{:keys [text]}]
                                      (if (= text "|")
                                        ""
                                        (subs text 1 (dec (count text))))))
  cm)

(defn serialize-selections!
  [^js cm]
  (replace-selections cm (fn [{:keys [text]}]
                           (if (= text "")
                             "|"
                             (str "<" text ">"))))
  cm)

(defn exec [^js cm command]
  (command cm)
  cm)

(defn test-exec [command pre-source]
  (.focus (editor))
  (.setValue (editor) (str/replace pre-source "'" \"))
  (-> (editor)
      (deserialize-selections!)
      (exec command)
      (serialize-selections!)
      (.getValue)
      (str/replace \" "'")))
