(ns lark.cards.structure.view
  (:require [chia.view :as v]
            [lark.cards.cm :as cm]
            [lark.cards.utils :as cu]
            [applied-science.js-interop :as j]
            [chia.jss :as jss]
            [lark.tree.emit :as emit]
            [chia.reactive.atom :as ra]
            [lark.cards.structure.samples :as samples]
            [lark.tree.core :as tree]
            [clojure.string :as str]
            [lark.structure.pointer :as pointer]))

(jss/classes! {"@global"
               {".hidden" {:position "absolute"
                           :left -9999999}
                ".no-wrap" {:white-space "nowrap"}
                ".cell" {:border "1px solid rgba(0,0,0,0.05)"}
                ".sample" {"&:hover, &.highlight" {:z-index 9
                                                   :border-color "rgba(0,0,0,0.1)"}}
                ".grid" {:display "grid"
                         :grid-template-columns "repeat(auto-fill, minMax(200px))"}
                ".sticky" {:position "sticky"}
                ".pre-wrap" {:white-space "pre-wrap"}
                "a" {"&:hover" {:text-decoration "underline"
                                :cursor "pointer"}}
                ".mono" {:font-family "Menlo, Monaco, \"Courier New\", monospace"}
                ".hover-underline:hover" {:text-decoration "underline"}}})

(defonce ui-state (atom {}))

(defn match-op? [op-key op-args op2]
  (= (subvec [(namespace op-key) (name op-key) (vec op-args)]
             0 (count op2))
     op2))

(v/defn op-header [op-key op-args]
  [:div.f7.b.mv2
   (when (and op-key (not op-args))
     [:<>
      [:a
       {:on-click #(swap! ui-state assoc :operation [(namespace op-key)])}
       (namespace op-key)]
      (when (namespace op-key)
        "/")
      [:a
       {:on-click #(swap! ui-state assoc :operation [(namespace op-key) (name op-key)])}
       (name op-key)]])
   (when op-args
     [:a.blue
      {:on-click #(swap! ui-state assoc :operation [(namespace op-key) (name op-key) (vec op-args)])}
      (str/join (map prn-str op-args))])])

(v/defn filter-bar []
  (let [{:as state
         :keys [operation]
         selected-sample :sample} (ra/deref ui-state)]
    (when-not (empty? (some->> (dissoc state :inspect)
                               (vals)
                               (keep identity)))
      [:.sticky.mono.bg-white.pa3.top-0.left-0.right-0.z-999.bb.b--near-white.f7
       (when-let [[ns n a] operation]
         [:a.mh2.blue.bg-near-white.pa1
          {:on-click #(swap! ui-state dissoc :operation)}
          (str ns (when n "/") n (when a " ") a)])
       (when selected-sample
         [:a.mh2.black
          {:on-click #(swap! ui-state dissoc :sample)}
          selected-sample])
       [:a.gray
        {:on-click #(reset! ui-state nil)} "clear"]])))

(def notes
  {:cursor/move
   "Need to handle beginning and end of lines."
   :+ch
   "

 - `#*` should be its own type? so `#{}` is not 'set' but rather \"sharp\" > \"map\".
   - problematic, a set's 'map' doesn't follow the rules of map.

 - general issue: insertions that change the node structure. `#js[]` > `#_js[]` > `#_#js[]` > `#_#_js[]`.
   each step has a different meaning. Some meanings would imply adding whitespace between
   `js` and `[]`.
    - the current


  "})

(defn show-editor-state [{:keys [ast
                                 selections]}]
  (let [spans (mapv (partial pointer/resolve-span (tree/zip ast)) selections)]
    [cm/editor
     (merge {:value (emit/string ast)
             :spans  spans})]))


(def error-count (atom 0))


(v/defn sample-inspect [{:keys [sample
                                state-before
                                state-after]}]
  [:.absolute.bottom-0.left-0
   [:.absolute.bg-gray.z-9999.no-wrap.f7.pa2.white
    {:style {:top "0.5rem"
             :left "-0.5rem"}}
    [:.mv1 (cu/show-selections (:selections state-before))]
    "--"
    [:.mv1 (cu/show-selections (:selections state-after))]
    "--"
    [:.mv1 (prn-str sample)]]])

(v/defn sample-error [{:as sample-state
                       :keys [source]
                       {:as state-after
                        :keys [error
                               failure]} :state-after}]
  (when (or error failure)
    (let [{:keys [error count]} state-after]
      [:.cell.bg-washed-red.f6.dark-red
       {:on-click #(do (js/console.log (str "#" count ": " source))
                       (js/console.error error))}
       (when count (str "#" count))
       (when (:expected error)
         [:div.f7.pa2.no-wrap
          [:div.mh1.mv2 "E: " (prn-str (:expected error))]
          [:div.mh1.mv2 "A: " (prn-str (:actual error))]])])))

(v/defn view-samples []
  (let [{:as state
         :keys [operation]
         inspect :inspect
         selected-sample :sample} (ra/deref ui-state)]
    [:<>
     [filter-bar]
     [:div.mono.ma3.f6
      (for [[op-key op-samples] samples/samples
            :let [samples-by-args (->> (for [[source tests] (partition 2 (flatten op-samples))
                                             [op-args expected] tests
                                             :when (or (nil? operation)
                                                       (match-op? op-key op-args operation))]
                                         [op-args source expected])
                                       (reduce (fn [m [op-args sample expected]]
                                                 (update m op-args (fnil conj []) [sample expected])) {}))
                  op-samples (for [[op-args tests] samples-by-args
                                   :let [sample-states (->> (for [[sample expected] tests
                                                                  :when (or (nil? selected-sample)
                                                                            (= sample selected-sample))]
                                                              (samples/operate-on-sample op-key op-args sample expected)))]
                                   :when (seq sample-states)]
                               {:op-args op-args
                                :sample-states sample-states})]
            :when (seq op-samples)]
        [:<>
         [:.mh2 [op-header op-key nil]]
         [:.flex.flex-wrap
          (for [{:keys [op-args
                        sample-states]} op-samples]
            [:div.mh2.mb3
             #_{:class (when-not op-visible? "hidden")}
             [op-header op-key op-args]
             [:.flex.flex-wrap.mh1
              (for [{:as sample-state
                     :keys [state-after
                            state-before
                            sample]} sample-states
                    :let [this [op-key op-args sample]
                          inspect? (= this inspect)
                          hidden? (and selected-sample
                                       (not= selected-sample sample))]]
                [:.sample.bw3.b--near-white.ba.nl1.nr1.nt1.nb1.relative
                 {:class (str (when (= this inspect)
                                "highlight ")
                              (when hidden?
                                "hidden"))
                  :on-click #(swap! ui-state assoc :sample sample)
                  :on-mouse-enter #(swap! ui-state assoc :inspect this)
                  :on-mouse-leave #(swap! ui-state update :inspect (fn [x] (if (= x this) nil x)))}
                 [:.pa2 [show-editor-state state-before]]
                 [:.bb.b--near-white]
                 [:.pa2 [show-editor-state state-after]]
                 #_[:div.mb2 (prn-str sample)]
                 #_[:div.mb2 (prn-str sample-expected)]
                 #_[:div.mb2 (prn-str sample-actual)]
                 [sample-error sample-state]
                 (when inspect?
                   [sample-inspect sample-state])])]])]])]]))