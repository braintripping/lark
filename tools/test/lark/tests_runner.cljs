(ns ^:figwheel-always lark.tests-runner
  (:require [lark.structure.edit-test]
            [cljs.test :refer-macros [run-all-tests]]))

(enable-console-print!)

(defn run-tests []
  (run-all-tests))

(run-tests)