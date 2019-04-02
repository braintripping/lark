(ns lark.structure.edit
  (:refer-clojure :exclude [char])
  (:require [lark.tree.core :as tree]
            [lark.tree.range :as range]
            [lark.tree.util :as util]
            [lark.tree.cursor :as cursor]
            [lark.editors.codemirror :as cm]
            [lark.fast-zip :as z]
            [goog.dom :as dom]
            [goog.dom.Range :as Range]
            [clojure.string :as string]
            [lark.tree.nav :as nav]
            [lark.tree.node :as node]
            [clojure.string :as str]
            [lark.tree.format :as format]
            [lark.tree.emit :as emit]
            [lark.tree.reader :as r]
            [chia.util :as u])
  (:require-macros [lark.structure.edit :as edit :refer [operation]]))

(defn format!
  ([editor] (format! editor {}))
  ([^js editor {:keys [preserve-cursor-space?]}]
   (let [source (.getValue editor)
         zipper (tree/string-zip source)
         pos (cm/pos->boundary (cm/get-cursor editor))
         cursor-loc (when preserve-cursor-space?
                      (nav/cursor-space-loc zipper pos))
         formatted-zipper (binding [r/*active-cursor-line* (when preserve-cursor-space?
                                                             (:line pos))]
                            (tree/format-zip zipper))
         formatted-source (:string (z/node formatted-zipper))]

     (when (not= source formatted-source)                   ;; only mutate editor if value has changed
       (.setValue editor formatted-source))

     (->> (cursor/path zipper pos cursor-loc)               ;; cursor path from pre-format zipper, ignoring whitespace
          (cursor/position formatted-zipper)                ;; returns position in post-format zipper for path
          (cm/range->Pos)
          (.setCursor editor))

     (cm/set-zipper! editor formatted-zipper))))

(defn apply-ast!
  [editor ast]
  (let [{:as new-ast
         :keys [string
                ast/cursor-pos]} (tree/format-ast ast)
        new-zipper (tree/zip new-ast)]

    (when (not= string (.getValue editor))                  ;; only mutate editor if value has changed
      (.setValue editor string))

    (some->> cursor-pos
             (cm/range->Pos)
             (.setCursor editor))

    (cm/set-zipper! editor new-zipper)))

(def other-bracket {\( \)
                    \[ \]
                    \{ \}
                    \" \"})

(defn spaces [n] (apply str (take n (repeat " "))))

(def clipboard-helper-element
  (memoize
   (fn []
     (let [textarea (doto (dom/createElement "pre")
                      (dom/setProperties #js {:id "lark-tree-pasteHelper"
                                              :contentEditable true
                                              :className "fixed o-0 z-0 bottom-0 right-0"}))]
       (dom/appendChild js/document.body textarea)
       textarea))))

(defn copy
  "Copy text to clipboard using a hidden input element."
  [text]
  (let [hadFocus (.-activeElement js/document)
        text (string/replace text #"[\n\r]" "<br/>")
        _ (aset (clipboard-helper-element) "innerHTML" text)]
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

(defn cursor-skip-pos
  [{{:keys [pos loc]} :magic/cursor} side]
  (let [move (case side :left nav/left-up
                        :right nav/right-up)
        nodes (->> (iterate move loc #_(nav/include-prefix-parents loc))
                   (take-while identity)
                   (map z/node)
                   (filter (fn [node]
                             (and (not (node/whitespace? node))
                                  (not (range/pos= pos (range/bounds node side)))))))]
    (some-> (first nodes)
            (range/bounds side))))

(defn cursor-skip!
  "Returns function for moving cursor left or right, touching only node boundaries."
  [cm side]
  (some->> (cursor-skip-pos cm side)
           (cm/set-cursor! cm)))

(defn move-char [cm pos amount]
  (.findPosH cm pos amount "char" false))

(defn char-at [cm pos]
  (.getRange cm pos (move-char cm pos 1)))

(defprotocol IPointer
  (get-range [this i])
  (move [this amount])
  (move-while! [this i pred])
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
              ch (cond-> (.-ch pos)
                         (= (.-line pos) (.. change -to -line)) (+ (-> (.-ch (cm/changeEnd change))
                                                                       (- (.. change -to -ch)))))]
          (cm/Pos line ch))))

(defn adjust-for-changes [pos changes]
  (loop [pos pos
         i 0]
    (if (= i (.-length changes))
      pos
      (recur (adjust-for-change pos (aget changes i))
             (inc i)))))

(defn move-while-pos [pos editor i pred]
  (loop [the-pos pos]
    (let [next-pos (move-char editor the-pos i)
          char (if (pos? i) (.getRange editor the-pos (move-char editor the-pos i))
                            (char-at editor next-pos))]
      (if (and (pred char) (not (.-hitSide next-pos)))
        (recur next-pos)
        the-pos))))

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
  (move-while! [this i pred]
    (set! pos (move-while-pos pos editor i pred))
    this)
  (move-while [this i pred]
    (assoc this :pos (move-while-pos pos editor i pred))))

(defn pointer
  ([editor] (pointer editor (cm/get-cursor editor)))
  ([editor pos] (->Pointer editor pos)))

(defn chars-around [the-pointer]
  (mapv (fn [i]
          (u/some-str (get-range the-pointer i))) [-1 1]))

(defn uneval! [{{:keys [loc]} :magic/cursor
                :as cm}]
  (when-let [loc (->> (cons (nav/include-prefix-parents loc) (nav/left-locs loc))
                      (remove (comp node/whitespace? z/node))
                      (first))]
    (let [node (z/node loc)]
      (let [a-pointer (pointer cm)
            changes (operation cm
                               (or (when-let [uneval-loc (first (filter (comp (partial = :uneval) :tag z/node)
                                                                        [loc (z/up loc)]))]
                                     (-> (pointer cm (cm/range->Pos (range/bounds (z/node uneval-loc) :left)))
                                         (insert! 2 "")))
                                   (-> (pointer cm (cm/range->Pos (range/bounds node :left)))
                                       (insert! "#_"))))]
        (adjust-for-changes! a-pointer changes)
        (set-editor-cursor! a-pointer))))
  true)

(def kill!
  (fn [{{pos :pos} :magic/cursor
        zipper :zipper :as editor}]
    (edit/with-formatting editor
      (let [loc (nav/navigate zipper pos)
            node (z/node loc)
            loc (cond-> loc
                        (or (not (range/within-inner? node pos))
                            (node/whitespace? node)) (z/up))
            node (z/node loc)
            in-edge? (when (node/has-edges? node)
                       (let [inner (range/inner-range node)]
                         (not (range/within? inner pos))))
            end-node (cond in-edge? nil                     ;; ignore kill when cursor is inside an edge structure, eg. #|""
                           (not (node/may-contain-children? node)) (range/inner-range node)

                           :else (->> (z/children loc)
                                      (drop-while #(range/lt (range/bounds % :right) pos))
                                      (take-while #(<= (:line %) (:line pos)))
                                      (last)))]
        (when end-node
          (->> (merge pos (select-keys end-node [:end-line :end-column]))
               (cut-range! editor)))))
    true))

(defn boundary? [s]
  (some->> (last s)
           (.indexOf "\"()[]{} ")
           (pos?)))

(defn unwrap! [{{:keys [pos loc bracket-node]} :magic/cursor :as editor}]
  (when (and loc (not (cm/selection? editor)))
    (when-let [edge-node (loop [loc (cond-> loc
                                            (not (range/within-inner? bracket-node pos)) (z/up))]
                           (cond (not loc) nil
                                 (node/has-edges? (z/node loc)) (z/node loc)
                                 :else (recur (z/up loc))))]
      (edit/with-formatting editor
        (let [[l r] (node/edges edge-node)
              [left-r right-r] (range/edge-ranges edge-node)]
          (doseq [[n range] [[(count l) left-r]
                             [(count r) right-r]]]
            (cm/replace-range! editor (format/spaces n) range))))))
  true)

(defn raise! [{{:keys [pos bracket-loc bracket-node]} :magic/cursor :as editor}]
  (when (and bracket-loc (z/up bracket-loc))
    (let [outer-node (z/node (z/up bracket-loc))]
      (edit/with-formatting editor
        (cm/replace-range! editor "" (range/end bracket-node) outer-node)

        (cm/replace-range! editor "" outer-node bracket-node))))
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
          item (first stack)]
      (swap! cm update-in [:magic/cursor :stack] (if (range/empty-range? item)
                                                   empty rest))
      item)))

(defn push-stack! [cm node]
  (when (range/empty-range? node)
    (swap! cm update-in [:magic/cursor :stack] empty))
  (when-not (= node (first (get-in cm [:magic/cursor :stack])))
    (swap! cm update-in [:magic/cursor :stack] conj (range/bounds node)))
  true)

(defn tracked-select [cm node]
  (when node
    (cm/select-range cm node)
    (push-stack! cm (range/bounds node))))

(defn push-cursor! [cm]
  (let [cursor (cm/get-cursor cm)]
    (push-stack! cm cursor)
    (cm/unset-temp-marker! cm)))

(def expand-selection
  (fn [{zipper :zipper
        :as cm}]
    (let [sel (cm/current-selection-bounds cm)
          loc (nav/navigate zipper sel)
          select! (partial tracked-select cm)
          cursor-root (cm/temp-marker-cursor-pos cm)
          selection? (cm/selection? cm)]
      (when (or cursor-root (not selection?))
        (push-cursor! cm)
        (push-stack! cm (cm/current-selection-bounds cm)))

      (loop [loc loc]
        (if-not loc
          sel
          (let [node (z/node loc)
                inner-range (when (node/has-edges? node)
                              (let [range (range/inner-range node)]
                                (when-not (range/empty-range? range)
                                  range)))]
            (cond (range/range= sel inner-range) (select! node)
                  (some-> inner-range
                          (range/within? sel)) (select! inner-range)
                  (range/range= sel node) (recur (z/up loc))
                  (range/within? node sel) (select! node)
                  :else (recur (z/up loc)))))))
    true))

(def shrink-selection
  (fn [cm]
    (some->> (pop-stack! cm)
             (cm/select-range cm))
    true))

(defn expand-selection-x [{zipper :zipper
                           :as cm} direction]
  (let [selection-bounds (cm/current-selection-bounds cm)
        selection-loc (nav/navigate zipper (range/bounds selection-bounds direction))
        selection-node (z/node selection-loc)
        cursor-root (cm/temp-marker-cursor-pos cm)]
    (when cursor-root
      (push-cursor! cm)
      (push-stack! cm selection-bounds))
    (if (and (node/has-edges? selection-node)
             (= (range/bounds selection-bounds direction)
                (range/bounds (range/inner-range selection-node) direction)))
      (expand-selection cm)

      (if-let [adjacent-loc (first (filter (comp (complement node/whitespace?) z/node) ((case direction :right nav/right-locs
                                                                                                        :left nav/left-locs) selection-loc)))]
        (tracked-select cm (merge (range/bounds (z/node adjacent-loc))
                                  (case direction :right (range/bounds selection-bounds :left)
                                                  :left (range/->end (range/bounds selection-bounds :right)))))
        (expand-selection cm))))
  true)

(def backspace! #(.execCommand % "delCharBefore"))

(defn comment-line
  ([cm]
   (operation
    cm
    (if (cm/selection? cm)
      (let [sel (aget (.listSelections cm) 0)
            [start end] (sort [(.. sel -anchor -line)
                               (.. sel -head -line)])]
        (doseq [line-n (range start (inc end))]
          (comment-line cm line-n)))
      (comment-line cm (.-line (cm/get-cursor cm))))))
  ([cm line-n]
   (let [[spaces semicolons] (rest (re-find #"^(\s*)(;+)?" (.getLine cm line-n)))
         [space-n semicolon-n] (map count [spaces semicolons])]
     (if (> semicolon-n 0)
       (cm/replace-range! cm "" {:line line-n
                                 :column space-n
                                 :end-column (+ space-n semicolon-n)})
       (cm/replace-range! cm ";;" {:line line-n
                                   :column space-n
                                   :end-column space-n})))
   true))

;; TODO
;; slurp/unslurp strings
;; - pad with space
;; - unslurp last spaced element

(defn slurp-parent? [node pos]
  (and (or #_(= :string (:tag node))
        (node/may-contain-children? node))
       (range/within-inner? node pos)))

(defn slurp-parent [loc pos]
  (loop [loc loc]
    (when loc
      (if (slurp-parent? (z/node loc) pos)
        loc
        (recur (z/up loc))))))

(def slurp-forward
  (fn [{{:keys [loc pos]} :magic/cursor
        :as cm}]
    (let [end-edge-loc (slurp-parent loc pos)
          end-edge-node (z/node end-edge-loc)
          start-edge-loc (nav/include-prefix-parents end-edge-loc)
          start-edge-node (z/node start-edge-loc)]
      (when (and start-edge-node
                 (not= :base (:tag start-edge-node)))
        (when-let [next-form (some->> (z/rights start-edge-loc)
                                      (remove node/whitespace?)
                                      first)]
          (let [form-content (-> start-edge-node
                                 (update :children conj next-form)
                                 (tree/format-ast)
                                 :string)
                range (merge (range/bounds end-edge-node :left)
                             (select-keys next-form [:end-line :end-column]))
                cur (.getCursor cm)]
            (cm/replace-range! cm form-content range)
            (.setCursor cm cur)))))
    true))

(def unslurp-forward
  (fn [{{:keys [loc pos]} :magic/cursor
        :as editor}]
    (let [end-edge-loc (slurp-parent loc pos)
          end-edge-node (some-> end-edge-loc z/node)]
      (when (and end-edge-node (not= :base (:tag end-edge-node)))
        (when-let [last-child (->> (z/children end-edge-loc)
                                   (remove node/whitespace?)
                                   (last))]
          (edit/with-formatting editor
            (-> (pointer editor (cm/range->Pos (range/end end-edge-node)))
                (insert! (str " " (emit/string last-child) " ")))
            (cm/replace-range! editor (-> (cm/range-text editor last-child)
                                          (str/replace #"[^\n]" " ")) last-child)
            (cm/set-cursor! editor (first (sort [(cm/range->Pos pos)
                                                 (-> (range/inner-range end-edge-node)
                                                     (range/end)
                                                     (cm/range->Pos))])))))))
    true))


(defn cursor-selection-edge [editor side]
  (cm/set-cursor! editor (-> (cm/current-selection-bounds editor)
                             (range/bounds side)))
  true)

(defn cursor-line-edge [editor side]
  (let [cursor (cm/get-cursor editor)
        line-i (.-line cursor)
        line (.getLine editor line-i)
        padding (count (second (re-find (case side :left #"^(\s+).*"
                                                   :right #".*?(\s+)$") line)))]
    (cm/set-cursor! editor (cm/Pos line-i (case side :left padding
                                                     :right (- (count line) padding)))))
  true)

(defn node-symbol [node]
  (when (= :token (.-tag node))
    (-> (emit/sexp node)
        (util/guard-> symbol?))))

(defn eldoc-symbol
  ([loc pos]
   (eldoc-symbol (cond-> loc
                         (= (range/bounds pos :left)
                            (some-> loc (z/node) (range/bounds :left))) (z/up))))
  ([loc]
   (some->> loc
            (nav/closest #(#{:list :fn} (.-tag (z/node %))))
            (z/children)
            (first)
            (node-symbol))))