(ns wavegen.test.html
  (:use [wavegen.core])
  (:use [wavegen.html])
  (:use [wavegen.aggr])
  (:use [clojure.test]))


(def replace-tokens (ns-resolve 'wavegen.html 'replace-tokens))
(deftest test-replace-tokens
         (is (= (replace-tokens "This is the ~A~ ~B~ thing ~C~" "~A~" "best" "~B~" "smelling" "~C~" "ever")
                "This is the best smelling thing ever")))

(def category (ns-resolve 'wavegen.html 'category))
(deftest test-category
    (let [sb (StringBuilder.)]
        (category sb "Category1" 0.5 {:A 1 :B 2 :C 3} [:A :B :C])
        (is (= (.toString sb) "<tr class='category'><td/><td colspan='4' class='category-name'>Category1</td><td/><td/><td class='category-weight'>0.5</td><td/><td/><td>1.0</td><td/><td/><td>2.0</td><td/><td/><td>3.0</td></tr>")
        )))
