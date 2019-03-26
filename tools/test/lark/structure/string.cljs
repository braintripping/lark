(ns lark.structure.string
  "String operations"
  (:require [chia.util :as u]
            [lark.structure.coords :as coords]
            [cljs.spec.alpha :as s]))

(defn line-count [s]
  (loop [lines 0
         index 0]
    (let [next-i (.indexOf s \newline index)]
      (if (= -1 next-i)
        lines
        (recur (inc lines)
               (inc next-i))))))

(s/fdef line-count
        :args (s/cat :s string?))

(defn- replace-at-index [s {:keys [from to]} to-insert]
  (let [to (or to from)]
    (str (subs s 0 from)
         to-insert
         (subs s to))))

(defn- line-index
  ([s line] (line-index s line 0))
  ([^string s line start]
   (assert (not (neg? line)))
   (loop [line line
          last-i start]
     (if (zero? line)
       last-i
       (let [next-i (.indexOf s \newline last-i)]
         (if (= -1 next-i)
           (.-length s)
           (recur (dec line)
                  (inc next-i))))))))

(defn- some-index-of [s index start]
  (let [i (.indexOf s index start)]
    (when-not (= -1 i)
      i)))

(defn- col-index [s column start]
  (let [next-line (or (some-index-of s \newline start)
                      (.-length s))]
    (max start
         (min (+ column start)
              next-line))))

(defn- pos-index [s [line column]]
  (let [line-i (line-index s line)
        col-i (col-index s column line-i)]
    col-i))

(defn replace-at-coords [s selection to-insert]
  (let [indexes (u/update-vals selection #(some->> % (pos-index s)))]
    (replace-at-index s indexes to-insert)))

(s/fdef replace-at-coords
        :args (s/cat :s string?
                     :sel ::coords/span
                     :insert string?))

(defn end-coords [s]
  (let [last-i (.lastIndexOf s \newline)]
    (if (= -1 last-i)
      [0 (.-length s)]
      [(line-count s)
       (- (.-length s) (inc last-i))])))

(s/fdef end-coords
        :args (s/cat :s string?))

(comment

 (let [text "a\nb\ncde"]
   (= (replace-at-coords
       text
       {:from [0 0]
        :to (end-coords text)}
       "Z")
      "Z"))

 (= (end-coords "a\n") [1 0])
 (= (end-coords "\n") [1 0])
 (= (end-coords "a") [0 1])
 (= (end-coords "\na") [1 1])

 ;; line index
 (assert (= (mapv (partial line-index "ABC\nD\nE")
                  [0 1 2 3 4])
            [0 4 6 7 7]))

 ;; col index
 (assert (= (mapv #(apply col-index "AB\nCD" %)
                  [[0 0]
                   [1 0]
                   [10 0]
                   [0 3]
                   [1 3]])
            [0 1 2 3 4]))

 (assert (= (insert-at-pos "a\n" [0 2] \x)
            "ax\n"))

 (assert (= (insert-at-pos "a\n" [1 0] \x)
            (insert-at-pos "a\n" [1 2] \x)
            (insert-at-pos "a\n" [2 0] \x)
            "a\nx"))

 (assert (= (insert-at-pos "a" [0 0] \x)
            "xa"))

 (assert (= (insert-at-pos "a" [0 1] \x)
            (insert-at-pos "a" [0 999] \x)
            "ax"))
 (assert (= (insert-at-pos "a\n" [0 1] \x)
            (insert-at-pos "a\n" [0 999] \x)
            "ax\n"))

 (assert (= (insert-at-pos "a\nb" [1 0] \x)
            "a\nxb"))

 (assert (= (insert-at-pos "a\nb" [1 1] \x)
            (insert-at-pos "a\nb" [1 999] \x)
            "a\nbx")))

