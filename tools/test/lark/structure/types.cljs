(ns lark.structure.types
  (:refer-clojure :exclude [Range]))

(defrecord Range [^number line ^number column])

(defrecord Selection [^Range from ^Range to])

(defrecord CursorPath [^List path ^Range offset])

