(ns lark.commands.exec
  (:require [goog.events :as events]
            [lark.commands.registry :as registry]
            [clojure.set :as set]))

(def debug? false)
(def context (volatile! {}))
(def last-selections (volatile! (list)))

(def state (atom {:modifiers-down    #{}
                  :which-key/active? false}))

(defn set-context! [values]
  (vswap! context merge values))

(def -before-exec (volatile! {}))
(defn before-exec [key cb]
  (vswap! -before-exec assoc key cb))

(def -context-augmenters (volatile! {}))
(defn add-context-augmenter!
  "Adds a reducing function which will be applied to the context before use."
  [key f]
  (vswap! -context-augmenters assoc key f))

(defn get-context
  ([] (get-context nil))
  ([event-attrs]
   (let [{:keys [block-view] :as context} @context]
     (let [augment-context (apply comp (vals @-context-augmenters))]
       (-> context
           (merge event-attrs)
           (augment-context))))))

(defn apply-context
  "Add contextual data to command"
  [context {:keys [exec-pred intercept-pred] :as command-entry}]
  (let [exec? (boolean (or (nil? exec-pred) (exec-pred context)))
        intercept? (if intercept-pred (intercept-pred context)
                                      exec?)]
    (assoc command-entry
      :exec? exec?
      :intercept? intercept?)))

(defn get-command
  "Returns command associated with name, if it exists.
  Add contextual data, :exec? and :intercept?."
  [context name]
  (apply-context context (get @registry/commands name)))

(defn contextual? [command]
  (or (:exec? command) (:intercept? command)))

(defn exec-command
  "Execute a command (returned by `get-command`)"
  [context {:keys [command exec? intercept? name]}]
  (let [result (when exec? (command context))]
    (when (and exec? debug?) (prn "Executed command:" name))
    (and result (not= result (.-Pass js/CodeMirror)))))

(defn exec-command-name
  ([name] (exec-command-name name (get-context)))
  ([name context]
   (exec-command context (get-command context name))))

(defn keyset-commands
  ([] (keyset-commands #{} (get-context)))
  ([modifiers-down current-context]
   (->> @registry/mappings
        (keep (fn [[keyset {:keys [exec]}]]
                (when (set/subset? modifiers-down keyset)
                  ;; change this later for multi-step keysets
                  (some->> (seq exec)
                           (map (partial get-command current-context))
                           (filter :exec?)))))
        (apply concat)
        (distinct))))

(defn contextual-commands
  [context]
  (->> (vals @registry/commands)
       (mapv #(apply-context context %))))

(def reverse-compare (fn [a b] (compare b a)))



(defn init-listeners []
  (let [clear-keys #(swap! state assoc :modifiers-down #{} :which-key/active? false)
        clear-timeout! #(some-> (:which-key/timeout @state) (js/clearTimeout))
        clear-which-key! #(do (clear-timeout!)
                              (swap! state assoc :which-key/active? false))

        which-key-delay 1500
        handle-keydown (fn [e]
                         (let [keycode (registry/normalize-keycode (.-keyCode e))
                               {which-key-active? :which-key/active?} @state
                               modifier? (contains? registry/modifiers keycode)
                               modifiers-down (registry/event-modifiers e)
                               keys-down (conj modifiers-down keycode)
                               command-names (seq (registry/get-keyset-commands keys-down))
                               context (when command-names
                                         (get-context {:keycode keycode
                                                       :key     (.-key e)}))
                               the-commands (when command-names
                                              (->> (map #(get-command context %) command-names)
                                                   (filter contextual?)
                                                   (sort-by :priority reverse-compare)))
                               _ (when the-commands
                                   (doseq [f (vals @-before-exec)]
                                     (f)))
                               ;; NOTE
                               ;; only running 1 command, sorting by priority.
                               results (when the-commands
                                         (take 1 (filter identity (map #(exec-command context %) the-commands))))]

                           (reset! state (cond-> @state
                                                 (and which-key-active? (= keycode 27))
                                                 (assoc :which-key/active? false)
                                                 the-commands
                                                 (assoc :last-exec-keys keys-down)))
                           ;; if we use Tab as a modifier,
                           ;; we'll stop its default behaviour
                           #_(when (= 9 keycode)
                               (.preventDefault e)
                               (.stopPropagation e))

                           (when (seq (filter identity results))
                             (.stopPropagation e)
                             (.preventDefault e)
                             (clear-which-key!))

                           (clear-timeout!)

                           (when modifier?
                             (swap! state assoc
                                    :modifiers-down modifiers-down
                                    :which-key/timeout (js/setTimeout
                                                         #(let [keys-down (:modifiers-down @state)]
                                                            (when (and (seq keys-down)
                                                                       (not= keys-down #{(registry/endkey->keycode "shift")}))
                                                              (swap! state assoc :which-key/active? true)))
                                                         which-key-delay)))))]

    (clear-keys)

    ;; NOTE: 'keydown' listeners MUST be activated in the 'capture' phase.
    ;;       Otherwise, editor state can change before the command is activated.

    (events/listen js/window "keydown" handle-keydown true)

    (events/listen js/window "keyup"
                   (fn [e]
                     (let [keycode (registry/normalize-keycode (.-keyCode e))]
                       (when (registry/modifiers keycode)
                         (let [modifiers-down (registry/event-modifiers e)]
                           (reset! state
                                   (cond-> (assoc @state :modifiers-down modifiers-down)
                                           (empty? modifiers-down) (assoc :which-key/active? false))))))) true)

    (events/listen js/window "mousedown"
                   (fn [e]
                     (doseq [command (-> (conj (:modifiers-down @state) (.-button e))
                                         (registry/get-keyset-commands))]
                       (exec-command-name command))))

    (events/listen js/window #js ["blur" "focus"] #(when (= (.-target %) (.-currentTarget %))
                                                     (clear-keys)))))

(defonce _ (init-listeners))