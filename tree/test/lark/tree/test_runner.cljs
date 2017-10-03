(ns lark.tree.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            #_[lark.tree.edit-test]
            [lark.tree.parse-test]))

(doo-tests #_'lark.tree.edit-test
           'lark.tree.parse-test)
