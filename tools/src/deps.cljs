{:foreign-libs [{:file           "js/codemirror.js"
                 :global-exports {cljsjs.codemirror CodeMirror}
                 :provides       ["cljsjs.codemirror" "codemirror"]}
                {:file     "js/codemirror.mode.clojure.js"
                 :provides ["codemirror.mode.clojure"]
                 :requires ["cljsjs.codemirror"]}
                {:file     "js/codemirror.addon.mark-selection.js"
                 :provides ["codemirror.addon.markselection"]
                 :requires ["cljsjs.codemirror"]}
                {:file     "js/codemirror.addon.search.searchcursor.js"
                 :provides ["codemirror.addon.search.searchcursor"]
                 :requires ["cljsjs.codemirror"]}
                {:file           "js/blank.js"
                 :provides       ["codemirror"]
                 :global-exports {codemirror CodeMirror}}]
                 :npm-deps {"keypress.js" "braintripping/Keypress#e29eb2e"}
                  :externs ["externs/lark.commands.ext.js"]}
