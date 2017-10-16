(ns lark.commands.registry
  (:require [goog.object :as gobj]
            [clojure.string :as string]
            [clojure.set :as set]
            [goog.events]
            ["keypress.js" :refer [keypress]]))

(defonce Keypress (new (.-Listener keypress)))

(def mac? (let [platform (.. js/navigator -platform)]
            (or (string/starts-with? platform "Mac")
                (string/starts-with? platform "iP"))))

(defn capitalize [s]
  (str (.toUpperCase (subs s 0 1)) (subs s 1)))

(defn format-segment [target modifier]
  (let [modifier (string/lower-case modifier)]
    (or (case target :Keypress.js (case modifier "m1" "meta"
                                                 "m2" "alt"
                                                 "`" "accent"
                                                 "tilde" "~"
                                                 modifier)
                     :internal (case modifier ("meta"
                                                "ctrl"
                                                "command") "m1"
                                              ("alt"
                                                "option") "m2"
                                              "accent" "`"
                                              "tilde" "~"
                                              modifier)
                     :display (let [modifier (format-segment :internal modifier)]
                                (case modifier
                                  "m1" (if mac? "⌘" "Ctrl")
                                  "m2" (if mac? "Option" "Alt")
                                  "left" "←"
                                  "right" "→"
                                  "up" "↑"
                                  "down" "↓"
                                  "backspace" "⌫"
                                  (capitalize modifier))))
        modifier)))

(def modifiers-internal #{"m1" "m2" "shift"})

(defn binding-string->vec [s]
  (mapv (partial format-segment :internal) (string/split s #"[\s-]")))

(defn binding-set [s]
  (set (binding-string->vec s)))

(defonce commands (atom {}))
(defonce mappings (atom {}))
(defonce handler (volatile! nil))

(defn M1-down? [e]
  (if mac? (.-metaKey e)
           (.-ctrlKey e)))

(defn get-keyset-commands
  "Returns command-names for a set of keys"
  [keyset]
  (get-in @mappings [keyset :exec]))

(defn spaced-name [the-name]
  (str (string/upper-case (first the-name)) (string/replace (subs the-name 1) "-" " ")))

(defn seq-disj
  "Removes `x` from `coll`"
  [coll x]
  (remove #(= % x) coll))

(defn distinct-conj
  "Conj `x` to coll, distinct"
  [coll x]
  (distinct (conj coll x)))

(defn- add-binding [mappings name binding]
  (let [binding-vec (binding-string->vec binding)
        path        [(set binding-vec) :exec]]
    (when-not (seq (get-in mappings path))
      (.register_combo Keypress #js {:keys        (to-array (mapv (partial format-segment :Keypress.js) binding-vec))
                                     :is_solitary true
                                     :on_keydown  #(@handler binding binding-vec)}))
    (update-in mappings path distinct-conj name)))

(defn- remove-binding [mappings name binding]
  (let [binding-vec (binding-string->vec binding)
        path        [(set binding-vec) :exec]
        mappings    (update-in mappings path seq-disj name)]
    (when-not (seq (get-in mappings path))
      (.unregister_combo Keypress #js {:keys (to-array (mapv (partial format-segment :Keypress.js) binding-vec))}))
    mappings))

(defn bind!
  "Takes a map of {<command-name>, <binding string>} and registers keybindings."
  [bindings]
  (let [[mappings* commands*] (reduce (fn [[mappings commands] [the-name binding]]
                                        [(add-binding mappings the-name binding)
                                         (update-in commands [the-name :bindings] (comp distinct conj) binding)])
                                      [@mappings @commands] bindings)]
    (reset! mappings mappings*)
    (reset! commands commands*)))

(defn unbind!
  "Takes a map of {<command-name>, <binding string>} and removes keybindings."
  [bindings]
  (let [[mappings* commands*] (reduce (fn [[mappings commands] [the-name binding]]
                                        [(remove-binding mappings the-name binding)
                                         (update-in commands [the-name :bindings] seq-disj binding)]) [@mappings @commands] bindings)]
    (reset! mappings mappings*)
    (reset! commands commands*)))

(defn register! [{the-name :name
                  priority :priority
                  :as      the-command} bindings]
  (swap! commands assoc the-name (merge the-command
                                        {:display-namespace (some-> (namespace the-name)
                                                                    (spaced-name))
                                         :display-name      (spaced-name (name the-name))
                                         :bindings          bindings
                                         :priority          (or priority 0)}))
  (reset! mappings (reduce (fn [mappings pattern]
                             (add-binding mappings the-name pattern)) @mappings bindings)))


(defn deregister! [the-name]
  (let [{:keys [bindings]} (get @commands the-name)]
    (unbind! (apply hash-map (interleave (repeat the-name) bindings)))
    (swap! commands dissoc the-name)))


(def sort-ks #(sort-by (fn [x] (if (string? x) x (:name (meta x)))) %))

(defn binding-segment-compare [segment]
  (if (#{"m1" "m2" "shift"} segment) 0 1))

(defn keyset-string [keyset]
  (let [modifiers #{"m1" "m2" "shift"}]
    (->> (sort-by binding-segment-compare (seq keyset))
         (mapv (partial format-segment :display))
         (interpose " ")
         (apply str))))