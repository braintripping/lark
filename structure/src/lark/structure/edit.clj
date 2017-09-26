(ns lark.structure.edit)

(defmacro operation
  "Wraps `body` in a CodeMirror operation, and returns the array of changes made by the operation."
  [editor & body]
  `(binding [~'lark.structure.edit/*changes* (~'array)]
     (~'.on ~editor "changes" ~'lark.structure.edit/log-editor-changes)
     (~'.operation ~editor (fn [] ~@body))
     (~'.off ~editor "changes" ~'lark.structure.edit/log-editor-changes)
     (or ~'lark.structure.edit/*changes* true)))