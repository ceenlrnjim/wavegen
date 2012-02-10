(ns wavegen.aggr
  (:use [wavegen.core]))

;
; Contains common functions that aggregate over the wave data structure to get
; derived information
;
; TODO: lots of calculations done over and over - need to optimize

(defn prod-keys
  "returns an ordered list of the product ids in the wave"
  [wave]
  (sort (keys (:products wave))))

(defn categories
  "Returns an ordered list of the top level categories"
  [wave]
  (sort ; order
    (set ; remove duplicates
      (map ; get categories from the requirements
        #(first (first %)) ; gets the key from the key/val vector, then the first element of the key is the category
        (group-by :categories (vals (:requirements wave)))))))


(defn subcategories
  "Returns an ordered sequence of the sub-categories in the specified category"
  [wave cat]
  (sort
    (set
      (map
        #(second (first %)) ; gets the key from key/val sequence, then the second element is the sub-category
        (filter 
          #(= (first (first %)) cat) ; select only those elements where the categories match
          (group-by :categories (vals (:requirements wave))))))))

(defn requirements
  "Returns a sequence of the requirements in the specified cat/subcat combination"
  [wave cat subcat]
  (get (group-by :categories (vals (:requirements wave))) [cat subcat]))

(defn reqt-weight
  "Returns the percentage of total weight for this requirement"
  [wave r]
  (let [total (reduce #(+ %1 (:wt %2)) 0 (vals (:requirements wave)))]
    (/ (:wt r) total)))

(defn weighted-score
  "Returns the weighted score of the specified product/requirement"
  [wave reqt-id prod-id]
  (* (reqt-weight wave (get (:requirements wave) reqt-id)) (get-score wave prod-id reqt-id)))

(defn subcat-weight
  "Returns the weight of the subcategory - the sum of the weights of the requirements in the sub category"
  [wave cat sub]
  (reduce
    #(+ %1 (reqt-weight wave %2))
    0
    (requirements wave cat sub)))

(defn subcat-scores
  "Returns a map keyed by product id mapping to the sum of all scores for the requirements in this subcategory"
  [wave cat subcat prod-ids]
  (reduce ; processes each product
    (fn [score-sum-map pid]
      (assoc 
        score-sum-map 
        pid 
        (reduce ; processes each requirement
          (fn [sum r]
            (+ sum (weighted-score wave (:id r) pid)))
          0
          (requirements wave cat subcat))))
    {}
    prod-ids))


(defn category-weight
  [wave cat]
  (reduce
    #(+ %1 (subcat-weight wave cat %2))
    0
    (subcategories wave cat)))

(defn category-scores
  "returns a map keyed by product id whose value is the sum of the subcategory scores for that product"
  [wave cat prod-ids]
  (reduce ; process each product
    (fn [score-sum-map pid]
      (assoc
        score-sum-map
        pid
        (reduce ; 3> Add together the score for each subcategory for the current product
          (fn [sum sc-scores]
            (+ sum (get sc-scores pid)))
          0
          (map ; 2> get scores for each subcategory
            #(subcat-scores wave cat % prod-ids) 
            (subcategories wave cat))))) ; 1> get all the subcategories
    {}
    prod-ids))

(defn total-scores
  "returns a map keyed by product id whose value is the total weighted score for the product"
  [wave prod-ids]
  (reduce
    (fn [score-sum-map pid]
      (assoc
        score-sum-map
        pid
        (reduce
          (fn [sum cat-scores]
            (+ sum (get cat-scores pid)))
          0
          (map
            #(category-scores wave % prod-ids)
            (categories wave)))))
    {}
    prod-ids))

