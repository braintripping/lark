(ns ^:figwheel-always lark.tests-runner
  (:require [lark.structure.bracket-tests]
            [lark.structure.edit-tests]
            [cljs.test :refer-macros [run-all-tests]]))

(enable-console-print!)

(defn run-tests []
  (run-all-tests))

(run-tests)