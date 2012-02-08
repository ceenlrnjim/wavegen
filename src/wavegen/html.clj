(ns wavegen.html
  (:use [wavegen.core]))


; {:type :header :product-names [Astera InfoSphere ...]} -> <tr class="header"><td>Req</td>...@@product-names@@
; {:type :category1 :weight 46.7 (derived) :computed-scores [1.456 2.498 ...] }
; {:type :category2 :weight 89.2 :computed-scores [...]}
; {:type :reqt :text "abc" :criteria {...} :weight 2.6 (computed to relative) :computed-scores [[1 0.1] [0 0]}
;
; probably need to nest reqts and categories first then flatten

; TODO: separate support functions that group and aggregate data from those responsible for generating html

;
; Data lookup and aggregation functions -----------------------------------------------------------
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

(defn- subcat-weight
  [wave cat sub]
  (reduce
    #(+ %1 (reqt-weight wave %2)) ; TODO: need to get normalized weights
    0
    (requirements wave cat sub)))

(defn- subcat-scores
  [wave cat subcat prod-ids]
  nil)


(defn- total-scores
  [wave prod-ids]
  nil)

(defn- category-weight
  [wave cat]
  (reduce
    #(+ %1 (subcat-weight wave cat %2))
    0
    (subcategories wave cat)))

(defn- category-scores
  [wave cat prod-ids]
  nil)


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

