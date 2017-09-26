(ns lark.structure.edit
  (:refer-clojure :exclude [char])
  (:require [magic-tree.core :as tree]
            [magic-tree.range :as range]
            [lark.structure.codemirror :as cm]
            [fast-zip.core :as z]
            [goog.dom :as dom]
            [goog.dom.Range :as Range]
            [clojure.string :as string])
  (:require-macros [lark.structure.edit :refer [operation]]))

(def other-bracket {\( \) \[ \] \{ \} \" \"})
(defn spaces [n] (apply str (take n (repeat " "))))

(def pass (.-Pass js/CodeMirror))

(def clipboard-helper-element
  (memoize (fn []
             (let [textarea (doto (dom/createElement "div")
                              (dom/setProperties #js {:id              "magic-tree.pasteHelper"
                                                      :contentEditable true
                                                      :className       "fixed pre o-0 z-0 bottom-0 right-0"}))]
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

(defn cursor-skip-bounds
  [{{:keys [pos loc]} :magic/cursor :as cm} side]
  (let [next-loc (case side :left z/left :right z/right)]
    (loop [loc loc]
      (cond (not= pos (tree/bounds (z/node loc) side)) (tree/bounds (z/node loc) side)
            (next-loc loc) (recur (next-loc loc))
            :else (some->> (z/up loc) recur)))))

(defn cursor-skip!
  "Returns function for moving cursor left or right, touching only node boundaries."
  [cm side]
  (some->> (cursor-skip-bounds cm side)
           (cm/set-cursor! cm)))

(defn move-char [cm pos amount]
  (.findPosH cm pos amount "char" false))

(defn char-at [cm pos]
  (.getRange cm pos (move-char cm pos 1)))

(defprotocol IPointer
  (get-range [this i])
  (move [this amount])
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
    (.setCursor editor pos)
    this)
  (adjust-for-changes! [this changes]
    (set! pos (adjust-for-changes pos changes))
    this))

(defn pointer
  ([editor] (pointer editor (cm/get-cursor editor)))
  ([editor pos] (->Pointer editor pos)))

(defn uneval! [{:keys [zipper] :as cm}]
  (let [selection-bounds (cm/selection-bounds cm)
        loc (tree/node-at zipper (tree/bounds selection-bounds :left))
        node (z/node loc)
        replace! (fn [original-loc replacement-node]
                   (let [loc (z/replace original-loc replacement-node)
                         string (tree/string (z/node loc))]
                     (cm/replace-range! cm string (z/node original-loc)))
                   true)]
    ;; TODO
    ;; after executing a command, the cursor/bracket locs do not update.
    (when loc
      (let [pointer (pointer cm)
            changes (operation cm
                               (or (and (= :uneval (:tag node))
                                        (replace! loc (first (:value node)))
                                        true)
                                   (and (= :uneval (some-> (z/up loc) (z/node) :tag))
                                        (replace! (z/up loc) (first (:value (z/node (z/up loc))))))
                                   (replace! loc {:tag   :uneval
                                                  :value [node]})))]
        (adjust-for-changes! pointer changes)
        (set-editor-cursor! pointer))))
  true)


(def kill!
  (fn [{{pos :pos} :magic/cursor
        zipper     :zipper :as cm}]
    (let [loc (tree/node-at zipper pos)
          node (z/node loc)
          loc (cond-> loc
                      (or (not (tree/inside? node pos))
                          (tree/whitespace? node)) (z/up))
          node (z/node loc)
          in-edge? (when-let [inner (tree/inner-range node)]
                     (not (tree/within? inner pos)))
          end-node (cond in-edge? nil                       ;; ignore kill when cursor is inside an edge structure, eg. #|""
                         (not (tree/may-contain-children? node)) (tree/inner-range node)

                         :else (let [next-nodes (->> (z/children loc)
                                                     (drop-while #(range/lt % pos)))
                                     next-newline (->> (take-while tree/whitespace? next-nodes)
                                                       (filter tree/newline?)
                                                       (first))]
                                 (or (when next-newline
                                       (->> (take-while tree/whitespace? next-nodes)
                                            (last)))
                                     (->> next-nodes
                                          (take-while (complement tree/newline?))
                                          (last)))))]
      (when end-node
        (->> (merge pos (select-keys end-node [:end-line :end-column]))
             (cut-range! cm))))
    true))

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
                   (cm/replace-range! cm (str (spaces (count l))
                                              (cm/range-text cm (tree/inner-range closest-edges-node))
                                              (spaces (count r)))
                                      closest-edges-node)
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
             pass
             (copy-range! cm (get-in cm [:magic/cursor :bracket-node])))))

(def cut-form
  (fn [cm] (if (cm/selection? cm)
             pass
             (cut-range! cm (get-in cm [:magic/cursor :bracket-node])))))

(def delete-form
  (fn [cm] (if (cm/selection? cm)
             pass
             (cm/replace-range! cm "" (get-in cm [:magic/cursor :bracket-node])))))

(def hop-left
  #(cursor-skip! % :left))

(def hop-right
  #(cursor-skip! % :right))

(defn pop-stack! [cm]
  (when-let [stack (get-in cm [:magic/cursor :stack])]
    (let [stack (cond-> stack
                        (or (:base (first stack))
                            (= (cm/selection-bounds cm) (first stack))) rest)
          item (first stack)]
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
  (cm/unset-cursor-root! cm))

(def expand-selection
  (fn [{{:keys [bracket-node] cursor-pos :pos} :magic/cursor
        zipper                                 :zipper
        :as                                    cm}]
    (let [sel (cm/selection-bounds cm)
          loc (tree/node-at zipper sel)
          node (z/node loc)
          select! (partial tracked-select cm)
          cursor-root (cm/cursor-root cm)
          selection? (cm/selection? cm)]
      (when (or cursor-root (not selection?))
        (push-cursor! cm)
        (push-stack! cm (cm/selection-bounds cm)))
      (loop [loc loc]
        (if-not loc
          sel
          (let [node (z/node loc)
                inner-range (tree/inner-range node)]
            (cond (range/pos= sel (tree/inner-range node)) (select! node)
                  (tree/within? inner-range sel) (select! inner-range)
                  (range/pos= sel node) (recur (z/up loc))
                  (tree/within? node sel) (select! node)
                  :else (recur (z/up loc)))))))
    true))

(def shrink-selection
  (fn [cm]
    (some->> (pop-stack! cm)
             (cm/select-range cm))
    true))

(defn expand-selection-left [{{:keys [bracket-node] pos :pos} :magic/cursor
                              zipper                          :zipper
                              :as                             cm}]
  (let [selection-bounds (cm/selection-bounds cm)
        selection-loc (tree/node-at zipper (tree/bounds selection-bounds :left))
        cursor-root (cm/cursor-root cm)]
    (when cursor-root
      (push-cursor! cm)
      (push-stack! cm selection-bounds))
    (if-let [left-loc (first (filter (comp (complement tree/whitespace?) z/node) (tree/left-locs selection-loc)))]
      (tracked-select cm (merge (tree/bounds (z/node left-loc) :left)
                                (select-keys selection-bounds [:end-line :end-column])))
      (expand-selection cm)))
  true)

(defn expand-selection-right [{{:keys [bracket-node] pos :pos} :magic/cursor
                               zipper                          :zipper
                               :as                             cm}]
  (let [selection-bounds (cm/selection-bounds cm)
        selection-loc (tree/node-at zipper (tree/bounds selection-bounds :right))
        cursor-root (cm/cursor-root cm)]
    (when cursor-root
      (push-cursor! cm)
      (push-stack! cm selection-bounds))
    (if-let [right-loc (first (filter (comp (complement tree/whitespace?) z/node) (tree/right-locs selection-loc)))]
      (tracked-select cm (merge (select-keys (z/node right-loc) [:end-line :end-column])
                                (tree/bounds selection-bounds :left)))
      (expand-selection cm)))
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
    (let [node (z/node loc)
          end-edge-loc (cond-> loc
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
    (let [node (z/node loc)
          end-edge-loc (cond-> loc
                               (or (= :string (:tag node))
                                   (not (slurp-parent? node pos))) z/up)
          end-edge-node (some-> end-edge-loc z/node)]
      (when (and end-edge-node (not= :base (:tag end-edge-node)))
        (when-let [last-inner-form (some->> (tree/child-locs end-edge-loc)
                                            (filter (comp tree/sexp? z/node))
                                            (last)
                                            (z/node))]
          (operation cm (let [right-bracket (second (tree/edges end-edge-node))]
                          (cm/replace-range! cm (str right-bracket " " (tree/string last-inner-form) " ")
                                             (merge (tree/bounds last-inner-form :left)
                                                    (select-keys end-edge-node [:end-line :end-column])))
                          (cm/set-cursor! cm pos))))))
    true))


(defn cursor-selection-edge [editor side]
  (cm/set-cursor! editor (-> (cm/selection-bounds editor)
                             (tree/bounds side)))
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