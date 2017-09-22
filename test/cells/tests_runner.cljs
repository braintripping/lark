(ns ^:figwheel-always cells.tests-runner
  (:require [cells.cell-tests]
            [goog.events :as events]
            [cljs.test :refer-macros [run-all-tests]]))

(enable-console-print!)

(defn run-tests []
  (run-all-tests #".*tests.*"))

(defonce _
         (do (run-tests)
             (events/listen js/document.body "figwheel.before-js-reload" run-tests)))