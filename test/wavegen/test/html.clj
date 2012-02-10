(ns wavegen.test.html
  (:use [wavegen.core])
  (:use [wavegen.html])
  (:use [clojure.test]))


(def prod-keys (ns-resolve 'wavegen.html 'prod-keys))
(deftest test-prod-keys
  (let [w (with-wave "test"
                     (reqt :one "1" 1 {} "A" "X")
                     (reqt :two "2" 1 {} "A" "X")
                     (product :A "Product A")
                     (product :B "Product A")
                     (score :A :one 1 :two 2)
                     (score :B :one 0 :two 0))]
    (is (= (prod-keys w) [:A :B]))))

(def categories (ns-resolve 'wavegen.html 'categories))
(deftest test-categories
  (let [w (with-wave "test"
                     (reqt :one "1" 1 {} "A" "X")
                     (reqt :two "2" 1 {} "A" "X")
                     (reqt :three "3" 1 {}  "A" "Y")
                     (reqt :four "4" 1 {}  "A" "Y")
                     (reqt :five "5" 1 {}  "B" "Z")
                     (reqt :six "6" 1 {}  "B" "Z")
                     (reqt :seven "7" 1 {}  "B" "Y")
                     (reqt :eight "8" 1 {}  "B" "Y"))]
    (is (= (categories w) ["A" "B"]))))

(def subcategories (ns-resolve 'wavegen.html 'subcategories))
(deftest test-subcategories
  (let [w (with-wave "test"
                     (reqt :one "1" 1 {} "A" "X")
                     (reqt :two "2" 1 {} "A" "X")
                     (reqt :three "3" 1 {}  "A" "Y")
                     (reqt :four "4" 1 {}  "A" "Y")
                     (reqt :five "5" 1 {}  "B" "Z")
                     (reqt :six "6" 1 {}  "B" "Z")
                     (reqt :seven "7" 1 {}  "B" "Y")
                     (reqt :eight "8" 1 {}  "B" "Y"))]
    (is (= (subcategories w "A") ["X" "Y"]))
    (is (= (subcategories w "B") ["Y" "Z"]))))

(def requirements (ns-resolve 'wavegen.html 'requirements))
(deftest test-requirements
  (let [w (with-wave "test"
                     (reqt :one "1" 1 {} "A" "X")
                     (reqt :two "2" 1 {} "A" "X")
                     (reqt :three "3" 1 {}  "A" "Y")
                     (reqt :four "4" 1 {}  "A" "Y")
                     (reqt :five "5" 1 {}  "B" "Z")
                     (reqt :six "6" 1 {}  "B" "Z")
                     (reqt :seven "7" 1 {}  "B" "Y")
                     (reqt :eight "8" 1 {}  "B" "Y"))]
    (let [rqts (requirements w "A" "X")]
      (is (= (count rqts) 2))
      (doseq [r rqts]
        (is (or (= (:id r) :one) (= (:id r) :two)))))
    (let [rqts (requirements w "B" "Z")]
      (is (= (count rqts) 2))
      (doseq [r rqts]
        (is (or (= (:id r) :five) (= (:id r) :six)))))))

(def reqt-weight (ns-resolve 'wavegen.html 'reqt-weight))
(deftest test-reqt-weight
  (let [w (with-wave "test"
                     (reqt :one "1" 2 {} "A" "X")
                     (reqt :two "2" 2 {} "A" "X")
                     (reqt :three "3" 1 {}  "A" "Y")
                     (reqt :four "4" 1 {}  "A" "Y")
                     (reqt :five "5" 1 {}  "B" "Z")
                     (reqt :six "6" 1 {}  "B" "Z")
                     (reqt :seven "7" 1 {}  "B" "Y")
                     (reqt :eight "8" 1 {}  "B" "Y"))]
    (is (= (double (reqt-weight w (:one (:requirements w)))) 0.2))
    (is (= (double (reqt-weight w (:two (:requirements w)))) 0.2))
    (is (= (double (reqt-weight w (:three (:requirements w)))) 0.1))
    (is (= (double (reqt-weight w (:seven (:requirements w)))) 0.1))))

(def subcat-weight (ns-resolve 'wavegen.html 'subcat-weight))
(deftest test-subcat-weight
  (let [w (with-wave "test"
                     (reqt :one "1" 2 {} "A" "X")
                     (reqt :two "2" 2 {} "A" "X")
                     (reqt :three "3" 1 {}  "A" "Y")
                     (reqt :four "4" 1 {}  "A" "Y")
                     (reqt :five "5" 1 {}  "B" "Z")
                     (reqt :six "6" 1 {}  "B" "Z")
                     (reqt :seven "7" 1 {}  "B" "Y")
                     (reqt :eight "8" 1 {}  "B" "Y"))]
    (is (= (double (subcat-weight w "A" "X")) 0.4))
    (is (= (double (subcat-weight w "A" "Y")) 0.2))
    (is (= (double (subcat-weight w "B" "Z")) 0.2))
    (is (= (double (subcat-weight w "B" "Y")) 0.2))))

(def weighted-score (ns-resolve 'wavegen.html 'weighted-score))
(deftest test-weighted-score
  (let [w (with-wave "test"
            (reqt :one "1" 3 {} "A" "A")
            (reqt :two "2" 1 {} "A" "A")
            (product :A "prodA")
            (score :A :one 1 :two 1))]
    (is (= (double (weighted-score w :one :A)) 0.75))
    (is (= (double (weighted-score w :two :A)) 0.25))))

(def subcat-scores (ns-resolve 'wavegen.html 'subcat-scores))
(deftest test-subcat-scores
  (let [w (with-wave "test"
            (reqt :one "1" 3 {} "A" "A")
            (reqt :two "2" 3 {} "A" "A")
            (reqt :three "3" 1 {} "A" "B")
            (reqt :four "4" 1 {} "A" "B")
            (product :prodA "prodA")
            (product :prodB "prodB")
            (score :prodA :one 1 :two 1 :three 1 :four 1)
            (score :prodB :one 0 :two 1 :three 0 :four 2))
        a-scores (subcat-scores w "A" "A" (prod-keys w))
        b-scores (subcat-scores w "A" "B" (prod-keys w))]
    (is (= (double (:prodA a-scores)) (double (+ (* (/ 3 8) 1) (* (/ 3 8) 1)))))
    (is (= (double (:prodB a-scores)) (double (+ (* (/ 3 8) 0) (* (/ 3 8) 1)))))
    (is (= (double (:prodA b-scores)) (double (+ (* (/ 1 8) 1) (* (/ 1 8) 1)))))
    (is (= (double (:prodB b-scores)) (double (+ (* (/ 1 8) 0) (* (/ 1 8) 2)))))))

(def category-scores (ns-resolve 'wavegen.html 'category-scores))
(deftest test-category-scores
  (let [w (with-wave "test"
            (reqt :one "1" 3 {} "A" "A")
            (reqt :two "2" 3 {} "A" "B")
            (reqt :three "3" 1 {} "A" "A")
            (reqt :four "4" 1 {} "A" "B")
            (product :prodA "prodA")
            (product :prodB "prodB")
            (score :prodA :one 1 :two 1 :three 1 :four 1)
            (score :prodB :one 0 :two 1 :three 0 :four 2))
        a-scores (category-scores w "A" (prod-keys w))
        prod-a-subcat-a-score (double (+ (* (/ 3 8) 1) (* (/ 1 8) 1)))
        prod-a-subcat-b-score (double (+ (* (/ 3 8) 1) (* (/ 1 8) 1)))
        prod-b-subcat-a-score (double (+ (* (/ 3 8) 0) (* (/ 1 8) 0)))
        prod-b-subcat-b-score (double (+ (* (/ 3 8) 1) (* (/ 1 8) 2)))]

    (is (= (double (:prodA a-scores)) (+ prod-a-subcat-a-score prod-a-subcat-b-score)))
    (is (= (double (:prodB a-scores)) (+ prod-b-subcat-a-score prod-b-subcat-b-score)))))

(def total-scores (ns-resolve 'wavegen.html 'total-scores))
(deftest test-total-scores
  (let [w (with-wave "test"
            (reqt :one "" 1 {} "c1" "c1s1")
            (reqt :two "" 1 {} "c1" "c1s2")
            (reqt :three "" 1 {} "c2" "c2s1")
            (reqt :four "" 1 {} "c2" "c2s2")
            (product :prodA "prodA")
            (product :prodB "prodB")
            (score :prodA :one 1 :two 1 :three 1 :four 1)
            (score :prodB :one 0 :two 2 :three 1 :four 1))
        totals (total-scores w (prod-keys w))]
    (is (= (double (:prodA totals)) 1.0))
    (is (= (double (:prodB totals)) 1.0))))


(def replace-tokens (ns-resolve 'wavegen.html 'replace-tokens))
(deftest test-replace-tokens
         (is (= (replace-tokens "This is the ~A~ ~B~ thing ~C~" "~A~" "best" "~B~" "smelling" "~C~" "ever")
                "This is the best smelling thing ever")))
