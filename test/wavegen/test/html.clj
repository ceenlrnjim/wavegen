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
