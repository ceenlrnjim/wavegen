(ns wavegen.html
  (:use [wavegen.core]))


; TODO: lots of calculations done over and over - need to optimize
; TODO: separate support functions that group and aggregate data from those responsible for generating html

;
; Data lookup and aggregation functions -----------------------------------------------------------
; TODO: move these functions back to core
;
(defn- prod-keys
  "returns an ordered list of the product ids in the wave"
  [wave]
  (sort (keys (:products wave))))

(defn- categories
  "Returns an ordered list of the top level categories"
  [wave]
  (sort ; order
    (set ; remove duplicates
      (map ; get categories from the requirements
        #(first (first %)) ; gets the key from the key/val vector, then the first element of the key is the category
        (group-by :categories (vals (:requirements wave)))))))


(defn- subcategories
  [wave cat]
  (sort
    (set
      (map
        #(second (first %)) ; gets the key from key/val sequence, then the second element is the sub-category
        (filter 
          #(= (first (first %)) cat) ; select only those elements where the categories match
          (group-by :categories (vals (:requirements wave))))))))

(defn- requirements
  "Returns a sequence of the requirements in the specified cat/subcat combination"
  [wave cat subcat]
  (get (group-by :categories (vals (:requirements wave))) [cat subcat]))

(defn- reqt-weight
  "Returns the percentage of total weight for this requirement"
  [wave r]
  (let [total (reduce #(+ %1 (:wt %2)) 0 (vals (:requirements wave)))]
    (/ (:wt r) total)))

(defn- weighted-score
  "Returns the weighted score of the specified product/requirement"
  [wave reqt-id prod-id]
  (* (reqt-weight wave (get (:requirements wave) reqt-id)) (get-score wave prod-id reqt-id)))

(defn- subcat-weight
  "Returns the weight of the subcategory - the sum of the weights of the requirements in the sub category"
  [wave cat sub]
  (reduce
    #(+ %1 (reqt-weight wave %2))
    0
    (requirements wave cat sub)))

(defn- subcat-scores
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


(defn- category-weight
  [wave cat]
  (reduce
    #(+ %1 (subcat-weight wave cat %2))
    0
    (subcategories wave cat)))

(defn- category-scores
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

(defn- total-scores
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


; TODO: move HTML snippets to resource file and use reader to load separate from code
;
; HTML generation functions ------------------------------------------------------------------------
;
(defn- page
  "renders the page level html"
  [output wave]
  (.append output "<html><head><link rel='stylesheet' href='wave.css'></head><body>"))

(def HEADER_LINE_1_HEADER "<tr class='header'><td colspan='5'></td><td colspan='3'>Weightings</td>")
(def HEADER_PRODUCT_NAME "<td colspan='3'>~NAME~</td>")
(def HEADER_LINE_1_TERMINATOR "</tr>")

(def HEADER_LINE_2_HEADER "<tr class='header'><td/><td>Category</td><td>Sub-category</td><td>Requirement</td><td>Evaluation Criteria</td><td>Reqt</td><td>Sub-cat</td><td>Category</td>")
(def HEADER_LINE_2_PRODUCT_HEADER "<td>Score</td><td>Notes</td><td>Wgt Score</td>")
(def HEADER_LINE_2_TERMINATOR "</tr>")

; TODO: can I create a protocol for these things that I can use for CSV, etc.?
(defn- header
  "Generates an html header row and appends to the specified string builder"
  [output wave prodkeys]
  ; Total cells = 8 * (3 + # products)
  (.append output HEADER_LINE_1_HEADER)
  (doseq [pid prodkeys]
    (.append output (.replace HEADER_PRODUCT_NAME "~NAME~" (:desc (get (:products wave) pid)))))
  (.append output HEADER_LINE_1_TERMINATOR)
  (.append output HEADER_LINE_2_HEADER)
  (doseq [pid prodkeys]
    (.append output HEADER_LINE_2_PRODUCT_HEADER))
  (.append output HEADER_LINE_2_TERMINATOR))

(def CATEGORY_HEADER "<tr class='category'><td/><td colspan='4' class='category-name'>~NAME~</td><td/><td/><td class='category-weight'>~WEIGHT~</td>")
(def CATEGORY_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
(def CATEGORY_TERMINATOR "</tr>")

(defn- category
  "scores is a map of product id to weighted score"
  [output cat weight scores prod-ids]
  (.append output (.replace (.replace CATEGORY_HEADER "~NAME~" cat) "~WEIGHT~" weight))
  (doseq [pid prod-ids]
    (.append output (.replace CATEGORY_PRODUCT_SCORE "~SCORE~" (get scores pid))))
  (.append output CATEGORY_TERMINATOR))


(defn- subcategory
  [output cat sub weights scores]
  nil)

(defn- requirement
  [output wave r prod-ids]
  nil)

(defn- totals
  [output scores]
  nil)

(defn- end-page
  [output wave]
  (.append output "</body></html"))

; TODO: modify to support arbitrary number of categories
(defn gen-html
  "returns a string containing the HTML representation of the wave"
  [wave]
  (let [output (StringBuilder.)
        prodlist (prod-keys wave)]
    (page output wave)
    (header output wave prodlist)
    (doseq [c (categories wave)]
      (category output c (category-weight wave c) (category-scores wave c prodlist) prodlist)
      (doseq [sc (subcategories wave c)]
        (subcategory output c sc (subcat-weight wave c sc) (subcat-scores wave c sc prodlist))
        (doseq [r (requirements wave c sc)]
          (requirement output wave r prodlist))))
    (totals output (total-scores wave prodlist))
    (end-page output wave)))

