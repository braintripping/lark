(ns lark.structure.edit
  (:refer-clojure :exclude [char])
  (:require [lark.tree.core :as tree]
            [lark.tree.range :as range]
            [lark.structure.codemirror :as cm]
            [fast-zip.core :as z]
            [goog.dom :as dom]
            [goog.dom.Range :as Range]
            [clojure.string :as string])
  (:require-macros [lark.structure.edit :refer [operation]]))

(def other-bracket {\( \) \[ \] \{ \} \" \"})
(defn spaces [n] (apply str (take n (repeat " "))))

(def clipboard-helper-element
  (memoize (fn []
             (let [textarea (doto (dom/createElement "pre")
                              (dom/setProperties #js {:id              "lark-tree-pasteHelper"
                                                      :contentEditable true
                                                      :className       "fixed o-0 z-0 bottom-0 right-0"}))]
               (dom/appendChild js/document.body textarea)
               textarea))))

(defn copy
  "Copy text to clipboard using a hidden input element."
  [text]
  (let [hadFocus (.-activeElement js/document)
        text     (string/replace text #"[\n\r]" "<br/>")
        _        (aset (clipboard-helper-element) "innerHTML" text)]
    (doto (Range/createFromNodeContents (clipboard-helper-element))
      (.select))
    (try (.execCommand js/document "copy")
         (catch js/Error e (.error js/console "Copy command didn't work. Maybe a browser incompatibility?")))
    (.focus hadFocus)))

(defn copy-range!
  "Copy a {:line .. :column ..} range from a CodeMirror instance."
  [cm range]
  (copy (cm/range-text cm range))
  true)

(defn cut-range!
  "Cut a {:line .. :column ..} range from a CodeMirror instance."
  [cm range]
  (copy (cm/range-text cm range))
  (cm/replace-range! cm "" range)
  true)

(defn cursor-skip-bounds
  [{{:keys [pos loc]} :magic/cursor :as cm} side]
  (let [traverse (case side :left z/left :right z/right)
        next-loc #(when-let [start (traverse %)]
                    (->> (iterate traverse start)
                         (keep identity)
                         (filter (comp (complement tree/whitespace?)
                                       z/node))
                         (first)))]
    (loop [loc loc]
      (let [node (z/node loc)]
        (if (and (not (tree/whitespace? node))
                 (not= pos (tree/bounds node side)))
          (tree/bounds (z/node loc) side)
          (if-let [loc (next-loc loc)]
            (recur loc)
            (some->> (z/up loc) recur)))))))

(defn cursor-skip!
  "Returns function for moving cursor left or right, touching only node boundaries."
  [cm side]
  (some->> (cursor-skip-bounds cm side)
           (cm/set-cursor! cm)))

(defn move-char [cm pos amount]
  (.findPosH cm pos amount "char" false))

(defn char-at [cm pos]
  (.getRange cm pos (move-char cm pos 1)))

(defn transpose-bounds [bounds dir]
  (case dir :end->start
            {:end-line   (:line bounds)
             :end-column (:column bounds)}
            :start->end
            {:line   (:end-line bounds)
             :column (:end-column bounds)}))

(defprotocol IPointer
  (get-range [this i])
  (move [this amount])
  (move-while [this i pred])
  (insert! [this s] [this replace-i s])
  (set-editor-cursor! [this])
  (adjust-for-changes! [this changes]))

(def ^:dynamic *changes* nil)

(defn log-editor-changes [cm changes]
  (when *changes*
    (.apply (.-push *changes*) *changes* changes)))

(defn adjust-for-change [pos change]
  (cond (<= (compare pos (.-from change)) 0) pos
        (<= (compare pos (.-to change)) 0) (cm/changeEnd change)
        :else
        (let [line (-> (.-line pos)
                       (+ (-> change .-text .-length))
                       (- (-> (.. change -to -line)
                              (- (.. change -from -line))))
                       (- 1))
              ch   (cond-> (.-ch pos)
                           (= (.-line pos) (.. change -to -line)) (+ (-> (.-ch (cm/changeEnd change))
                                                                         (- (.. change -to -ch)))))]
          (cm/Pos line ch))))

(defn adjust-for-changes [pos changes]
  (loop [pos pos
         i   0]
    (if (= i (.-length changes))
      pos
      (recur (adjust-for-change pos (aget changes i))
             (inc i)))))

(defrecord Pointer [editor ^:mutable pos]
  IPointer
  (get-range [this i]
    (if (neg? i)
      (.getRange editor (:pos (move this i)) pos)
      (.getRange editor pos (:pos (move this i)))))
  (move [this amount]
    (assoc this :pos (move-char editor pos amount)))
  (insert! [this text]
    (.replaceRange editor text pos pos)
    this)
  (insert! [this amount text]
    (.replaceRange editor text pos (move-char editor pos amount))
    this)
  (set-editor-cursor! [this]
    (.setCursor editor pos nil #js {:scroll false})
    this)
  (adjust-for-changes! [this changes]
    (set! pos (adjust-for-changes pos changes))
    this)
  (move-while [this i pred]
    (loop [the-pos pos]
      (let [next-pos (move-char editor the-pos i)
            char     (char-at editor next-pos)]
        (if (and (pred char) (not (.-hitSide next-pos)))
          (recur next-pos)
          (do (set! pos the-pos)
              this))))))

(defn pointer
  ([editor] (pointer editor (cm/get-cursor editor)))
  ([editor pos] (->Pointer editor pos)))

(defn loc-splice [loc items]
  (let [loc (reduce (fn [loc item]
                      (z/insert-left loc item)) loc items)]
    (z/remove loc)))

(defn closest-left [pred loc]
  (when loc
    (if (pred (z/node loc))
      loc
      (some->> (z/left loc)
               (recur pred)))))

(defn uneval! [{:keys [zipper] :as cm}]
  (let [selection-bounds (cm/current-selection-bounds cm)]
    (when-let [loc (->> (tree/node-at zipper (tree/bounds selection-bounds :left))
                        (cm/cursor-loc (:pos (:magic/cursor cm)))
                        (closest-left (complement tree/whitespace?)))]
      (let [node     (z/node loc)
            replace! (fn [target-loc items uneval?]
                       (let [string (tree/string {:tag   (if uneval? :uneval :base)
                                                  :value items})]
                         (cm/replace-range! cm string (z/node target-loc)))
                       true)]
        (let [pointer (pointer cm)
              changes (operation cm
                                 (or (and (= :uneval (:tag node))
                                          (replace! loc (:value node) false)
                                          true)
                                     (and (= :uneval (some-> (z/up loc) (z/node) :tag))
                                          (replace! (z/up loc) (:value (z/node (z/up loc))) false))
                                     (replace! loc [node] true)))]
          (adjust-for-changes! pointer changes)
          (set-editor-cursor! pointer)))))
  true)

(def kill!
  (fn [{{pos :pos} :magic/cursor
        zipper     :zipper :as cm}]
    (let [loc      (tree/node-at zipper pos)
          node     (z/node loc)
          loc      (cond-> loc
                           (or (not (tree/inside? node pos))
                               (tree/whitespace? node)) (z/up))
          node     (z/node loc)
          in-edge? (when (tree/has-edges? node)
                     (let [inner (tree/inner-range node)]
                       (not (tree/within? inner pos))))
          end-node (cond in-edge? nil                       ;; ignore kill when cursor is inside an edge structure, eg. #|""
                         (not (tree/may-contain-children? node)) (tree/inner-range node)

                         :else (->> (z/children loc)
                                    (drop-while #(range/lt (range/bounds % :right) pos))
                                    (take-while #(<= (:line %) (:line pos)))
                                    (last)))]
      (when end-node
        (->> (merge pos (select-keys end-node [:end-line :end-column]))
             (cut-range! cm))))
    true))

(defn boundary? [s]
  (some->> (last s)
           (.indexOf "\"()[]{} ")
           (pos?)))

(defn unwrap! [{{:keys [pos bracket-loc bracket-node]} :magic/cursor :as cm}]
  (when (and bracket-loc (not (cm/selection? cm)))
    (when-let [closest-edges-node (loop [loc (cond-> bracket-loc
                                                     (not (tree/inside? bracket-node pos)) (z/up))]
                                    (cond (not loc) nil
                                          (tree/has-edges? (z/node loc)) (z/node loc)
                                          :else (recur (z/up loc))))]
      (let [pos (cm/get-cursor cm)
            [l r] (tree/edges closest-edges-node)]
        (operation cm
                   (let [inner-text (cm/range-text cm (tree/inner-range closest-edges-node))

                         ;; if the inner-text ends in a non-boundary character,
                         ;; add padding to avoid merging symbols.
                         ;; TODO
                         ;; a more principled approach
                         pad-right? (some-> (last inner-text)
                                            (boundary?)
                                            (not))]
                     (cm/replace-range! cm (str (spaces (count l))
                                                inner-text
                                                (when pad-right?
                                                  (spaces (count r))))
                                        closest-edges-node))
                   (cm/set-cursor! cm pos)))

      true))
  true)

(defn raise! [{{:keys [pos bracket-loc bracket-node]} :magic/cursor :as cm}]
  ;; TODO
  ;; highlight bracket node for raise
  (when (and bracket-loc (z/up bracket-loc))
    (let [up (z/node (z/up bracket-loc))]
      (operation cm
                 (cm/replace-range! cm (tree/string bracket-node) up)
                 (cm/set-cursor! cm (tree/bounds up :left)))))
  true)

(def copy-form
  (fn [cm] (if (cm/selection? cm)
             :lark.commands/Pass
             (copy-range! cm (get-in cm [:magic/cursor :bracket-node])))))

(def cut-form
  (fn [cm] (if (cm/selection? cm)
             :lark.commands/Pass
             (cut-range! cm (get-in cm [:magic/cursor :bracket-node])))))

(def delete-form
  (fn [cm] (if (cm/selection? cm)
             :lark.commands/Pass
             (cm/replace-range! cm "" (get-in cm [:magic/cursor :bracket-node])))))

(defn pop-stack! [cm]
  (when-let [stack (get-in cm [:magic/cursor :stack])]
    (let [stack (cond-> stack
                        (or (:base (first stack))
                            (= (cm/current-selection-bounds cm) (first stack))) rest)
          item  (first stack)]
      (swap! cm update-in [:magic/cursor :stack] (if (tree/empty-range? item)
                                                   empty rest))
      item)))

(defn push-stack! [cm node]
  (when (tree/empty-range? node)
    (swap! cm update-in [:magic/cursor :stack] empty))
  (when-not (= node (first (get-in cm [:magic/cursor :stack])))
    (swap! cm update-in [:magic/cursor :stack] conj (tree/bounds node)))
  true)

(defn tracked-select [cm node]
  (when node
    (cm/select-range cm node)
    (push-stack! cm (tree/bounds node))))

(defn push-cursor! [cm]
  (push-stack! cm (cm/Pos->range (cm/get-cursor cm)))
  (cm/unset-temp-marker! cm))

(def expand-selection
  (fn [{zipper :zipper
        :as    cm}]
    (let [sel         (cm/current-selection-bounds cm)
          loc         (tree/node-at zipper sel)
          select!     (partial tracked-select cm)
          cursor-root (cm/temp-marker-cursor-pos cm)
          selection?  (cm/selection? cm)]
      (when (or cursor-root (not selection?))
        (push-cursor! cm)
        (push-stack! cm (cm/current-selection-bounds cm)))
      (loop [loc loc]
        (if-not loc
          sel
          (let [node        (z/node loc)
                inner-range (when (tree/has-edges? node)
                              (let [range (tree/inner-range node)]
                                (when-not (tree/empty-range? range)
                                  range)))]
            (cond (range/pos= sel inner-range) (select! node)
                  (some-> inner-range
                          (tree/within? sel)) (select! inner-range)
                  (range/pos= sel node) (recur (z/up loc))
                  (tree/within? node sel) (select! node)
                  :else (recur (z/up loc)))))))
    true))

(def shrink-selection
  (fn [cm]
    (some->> (pop-stack! cm)
             (cm/select-range cm))
    true))

(defn expand-selection-x [{zipper :zipper
                           :as    cm} direction]
  (let [selection-bounds (cm/current-selection-bounds cm)
        selection-loc    (tree/node-at zipper (tree/bounds selection-bounds direction))
        selection-node   (z/node selection-loc)
        cursor-root      (cm/temp-marker-cursor-pos cm)]
    (when cursor-root
      (push-cursor! cm)
      (push-stack! cm selection-bounds))
    (if (and (tree/has-edges? selection-node)
             (= (tree/bounds selection-bounds direction)
                (tree/bounds (tree/inner-range selection-node) direction)))
      (expand-selection cm)

      (if-let [adjacent-loc (first (filter (comp (complement tree/whitespace?) z/node) ((case direction :right tree/right-locs
                                                                                                        :left tree/left-locs) selection-loc)))]
        (tracked-select cm (merge (z/node adjacent-loc)
                                  (case direction :right (tree/bounds selection-bounds :left)
                                                  :left (tree/shift-end (tree/bounds selection-bounds :right)))))
        (expand-selection cm))))
  true)

(def backspace! #(.execCommand % "delCharBefore"))

(def comment-line
  (fn [{zipper :zipper :as cm}]
    (operation cm
               (let [{line-n :line column-n :column} (get-in cm [:magic/cursor :pos])
                     [spaces semicolons] (rest (re-find #"^(\s*)(;+)?" (.getLine cm line-n)))
                     [space-n semicolon-n] (map count [spaces semicolons])]
                 (if (> semicolon-n 0)
                   (cm/replace-range! cm "" {:line line-n :column space-n :end-column (+ space-n semicolon-n)})
                   (let [{:keys [end-line end-column]} (some-> (tree/node-at zipper {:line line-n :column 0})
                                                               z/up
                                                               z/node)]
                     (when (= line-n end-line)
                       (cm/replace-range! cm (str "\n" spaces) {:line line-n :column (dec end-column)}))
                     (cm/replace-range! cm ";;" {:line line-n :column space-n})))
                 (.setCursor cm (cm/Pos (inc line-n) column-n))))
    true))

(defn slurp-parent? [node pos]
  (and (or (= :string (:tag node))
           (tree/may-contain-children? node))
       (tree/inside? node pos)))

(def slurp-forward
  (fn [{{:keys [loc pos]} :magic/cursor
        :as               cm}]
    (let [node           (z/node loc)
          end-edge-loc   (cond-> loc
                                 (not (slurp-parent? node pos)) z/up)
          start-edge-loc (tree/include-prefix-parents end-edge-loc)
          {:keys [tag] :as node} (z/node start-edge-loc)]
      (when (and node (not= :base tag))
        (when-let [next-form (some->> (tree/right-locs start-edge-loc)
                                      (filter (comp tree/sexp? z/node))
                                      first
                                      (z/node))]
          (operation cm (let [right-bracket (second (tree/edges (z/node end-edge-loc)))]
                          (cm/replace-range! cm (or right-bracket "") (tree/bounds next-form :right))
                          (cm/replace-range! cm "" (-> (tree/bounds node :right)
                                                       (assoc :end-column (dec (:end-column node))))))))))
    true))

(def unslurp-forward
  (fn [{{:keys [loc pos]} :magic/cursor
        :as               cm}]
    (let [node          (z/node loc)
          end-edge-loc  (cond-> loc
                                (or (= :string (:tag node))
                                    (not (slurp-parent? node pos))) z/up)
          end-edge-node (some-> end-edge-loc z/node)]
      (when (and end-edge-node (not= :base (:tag end-edge-node)))
        (let [inner-forms (some->> (z/children end-edge-loc)
                                   (filter tree/sexp?)
                                   (reverse))]
          (when-let [last-inner-form (first inner-forms)]
            (operation cm (let [right-bracket (second (tree/edges end-edge-node))
                                range         (merge (or (some-> (second inner-forms)
                                                                 (tree/bounds :right))
                                                         (tree/bounds last-inner-form :left))
                                                     (select-keys end-edge-node [:end-line :end-column]))
                                add-padding?  (not= " " (char-at cm (cm/range->Pos
                                                                      (update (transpose-bounds range :end->start)
                                                                              :column inc))))]
                            (cm/replace-range! cm (str right-bracket " " (tree/string last-inner-form)
                                                       (when add-padding? " "))
                                               range)
                            (cm/set-cursor! cm pos)))))))
    true))


(defn cursor-selection-edge [editor side]
  (cm/set-cursor! editor (-> (cm/current-selection-bounds editor)
                             (tree/bounds side)))
  true)

(defn cursor-line-edge [editor side]
  (let [cursor  (cm/get-cursor editor)
        line-i  (.-line cursor)
        line    (.getLine editor line-i)
        padding (count (second (re-find (case side :left #"^(\s+).*"
                                                   :right #".*?(\s+)$") line)))]
    (cm/set-cursor! editor (cm/Pos line-i (case side :left padding
                                                     :right (- (count line) padding)))))
  true)

(defn sym-node [node]
  (when (= :symbol (:tag node))
    (tree/sexp node)))

(defn eldoc-symbol
  ([loc pos]
   (eldoc-symbol (cond-> loc
                         (= (tree/bounds pos :left)
                            (some-> loc (z/node) (tree/bounds :left))) (z/up))))
  ([loc]
   (some->> loc
            (tree/closest #(#{:list :fn} (:tag (z/node %))))
            (z/children)
            (first)
            (sym-node))))