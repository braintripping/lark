(ns ^:figwheel-always tests.tests-runner
  (:require [cells.cell-tests]
            [lark.structure.bracket-tests]
            [lark.structure.edit-tests]
            [cljs.test :refer-macros [run-all-tests]]
            [doo.runner :refer-macros [doo-all-tests]]))

(enable-console-print!)

(defn run-tests []
  (doo-all-tests ))

(defonce _
         (do (run-tests)
             (.addEventListener js/document.body "figwheel.before-js-reload" run-tests)))