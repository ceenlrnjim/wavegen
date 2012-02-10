(ns wavegen.test.html
  (:use [wavegen.core])
  (:use [wavegen.html])
  (:use [wavegen.aggr])
  (:use [clojure.test]))


(def replace-tokens (ns-resolve 'wavegen.html 'replace-tokens))
(deftest test-replace-tokens
         (is (= (replace-tokens "This is the ~A~ ~B~ thing ~C~" "~A~" "best" "~B~" "smelling" "~C~" "ever")
                "This is the best smelling thing ever")))
