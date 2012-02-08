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
