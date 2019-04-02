(ns lark.keys
  (:require [applied-science.js-interop :as j]))

(defn modifiers [e]
  (cond-> #{}
          (j/get e :metaKey) (conj :meta)
          (j/get e :ctrlKey) (conj :ctrl)
          (j/get e :altKey) (conj :alt)
          (j/get e :shiftKey) (conj :shift)))

(def unsafe-combos
  (set
   (concat
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; combos that are blocked in at least 1 major browser

    ;; SAFARI
    (for [n (range 10)]
      #{:meta n})

    (for [ch ["t"                                           ;; new tab
              "n"                                           ;; new window
              "m"                                           ;; minimize
              "h"                                           ;; hide
              "q"                                           ;; quit
              ]]
      #{:meta ch})

    ;; CHROME
    (for [arrow ["ArrowLeft"                                ;; move tab
                 "ArrowRight"]]
      #{:meta :alt arrow})

    [#{:meta :shift "w"}                                    ;; close window
     #{:meta :shift "t"}                                    ;; open last tab
     ]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; combos that can be bound, but (maybe) shouldn't

    [#{:meta :shift "n"}                                    ;; new private window
     #{:meta "-"} #{:meta "="}                              ;; zoom in/out
     #{:meta "r"}                                           ;; refresh
     ]

    )))




(comment
 ;; TBD - click handling
 #{:ctrl :click/left}                                       ;; open link in new tab / background
 #{:ctrl :shift :click/left})                               ;; open link in new tab + switch


;; Observations
;; - Meta+Number, move to tab
;; - Meta+T, new tab
;; - Meta+N, new window
;; - Meta+M, minimize
;; - Meta-H, hide all