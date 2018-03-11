(ns lark.structure.edit)

(def edit 'edit)

(defmacro operation
  "Wraps `body` in a CodeMirror operation, and returns the array of changes made by the operation."
  [editor & body]
  `(binding [~'lark.structure.edit/*changes* (~'array)]
     (~'.on ~editor "changes" ~'lark.structure.edit/log-editor-changes)
     (~'.operation ~editor (fn [] ~@body))
     (~'.off ~editor "changes" ~'lark.structure.edit/log-editor-changes)
     (or ~'lark.structure.edit/*changes* true)))

(defmacro with-formatting
  [editor & body]
  ;; TODO
  ;; - a `format` command which returns a "formatted ast"
  ;; - set contents of editor via AST instead of string
  ;; first, eval `body`. assume that cursor is left in correct position.
  `(let [res# (do ~@body)]
     (when res#
       (~'lark.structure.edit/format! ~editor))
     res#))