(ns wavegen.test.core
  (:use [wavegen.core])
  (:use [clojure.test]))

(def yesno { 1 "Yes" 0 "No" })

(def normalize-weights (ns-resolve 'wavegen.core 'normalize-weights))
(deftest test-normalize-weights
  (let [w (with-wave "test wave"
                     (reqt :one "One" 1 yesno "Category1" "SubcategoryA")
                     (reqt :two "Two" 1 yesno "Category1" "SubcategoryA")
                     (reqt :three "three" 1 yesno "Category1" "SubcategoryA")
                     (reqt :four "Four" 1 yesno "Category1" "SubcategoryA"))
        weights (normalize-weights (vals (:requirements w)))]
    (is (= 0.25 (float (:one weights))))
    (is (= 0.25 (float (:two weights))))
    (is (= 0.25 (float (:three weights))))
    (is (= 0.25 (float (:four weights))))))


(def compute-product-scores (ns-resolve 'wavegen.core 'compute-product-scores))
(deftest test-compute-product-scores
  (let [w (with-wave "test"
                     (reqt :one "1" 1 yesno "A" "X")
                     (reqt :two "2" 1 yesno "A" "X")
                     (product :A "Product A")
                     (score :A :one 1 :two 2))]
    (is (= [0.5 1] (compute-product-scores w :one 0.5)))
    (is (= [1 2] (compute-product-scores w :two 0.5)))))
