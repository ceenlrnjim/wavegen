(ns wavegen.html
  (:use [wavegen.core]))
  ;(:use [wavegen.aggr]))

(def formatter (java.text.DecimalFormat. "#0.0#"))
(defn decfmt 
  "Returns the specific number as a standard format string"
  [d] (.format formatter d))

(defn template-map
  "takes a template with delimited placeholders and fills the placeholders with the values in the map"
  [template m]
  (apply str (map #(if (keyword? %) (get m %) %) template)))

(defn parent-child-template-map
  "parent template, child template, child key (in parent), map"
  [pt ct ck m]
  (let [parent (template-map pt m)
        children (map #(template-map ct %) (get m ck))] ; note this assumes the value is a seq of maps
    (apply str parent children)))

; TODO: move HTML snippets to resource file and use reader to load separate from code
;(def HEADER_LINE_1_HEADER "<tr class='header'><td colspan='5'></td><td colspan='3'>Weightings</td>")
;(def HEADER_PRODUCT_NAME "<td colspan='3'>~NAME~</td>")
;(def HEADER_LINE_1_TERMINATOR "</tr>\n")
;
;(def HEADER_LINE_2_HEADER "<tr class='header'><td class='linenumlabel'></td><td class='categorylabel'>Category</td><td class='subcategorylabel'>Sub-category</td><td class='reqtlabel'>Requirement</td><td class='criterialabel'>Evaluation Criteria</td><td class='reqtwtlabel'>Reqt</td><td class='subcatwtlabel'>Sub-cat</td><td class='catwtlabel'>Category</td>")
;(def HEADER_LINE_2_PRODUCT_HEADER "<td class='scorelabel'>Score</td><td class='noteslabel'>Notes</td><td class='wtscorelabel'>Wgt Score</td>")
;(def HEADER_LINE_2_TERMINATOR "</tr>\n")
;
;(def CATEGORY_HEADER "<tr class='category'><td class='linenum'>~LINE~</td><td colspan='4' class='category-name'>~NAME~</td><td/><td/><td class='category-weight'>~WEIGHT~</td>")
;(def CATEGORY_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
;(def CATEGORY_TERMINATOR "</tr>\n")
;
;(def SUBCATEGORY_HEADER "<tr class='subcategory'><td class='linenum'>~LINE~</td><td/><td colspan='3' class='subcategory-name'>~NAME~</td><td/><td class='subcategory-weight'>~WEIGHT~</td><td/>")
;(def SUBCATEGORY_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
;(def SUBCATEGORY_TERMINATOR "</tr>\n")

;(def REQT_HEADER "<tr class='requirement'><td class='linenum'>~LINE~</td><td/><td/><td>~:reqtdesc~</td><td class='criteria'>~:scores~</td><td>~:abs-weight~</td><td/><td/>")
;(def REQT_PRODUCT_SCORE "<td>~:raw~</td><td/><td>~:score~</td>")
;(def REQT_TERMINATOR "</tr>\n")
;
;(def TOTAL_HEADER "<tr class='total'><td/><td/><td/><td/><td colspan='4'>Final Score:</td>")
;(def TOTAL_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
;(def TOTAL_TERMINATOR "</tr>\n")

(def HEADER_LINE_1_HEADER ["<tr class='header'><td colspan='5'></td><td colspan='3'>Weightings</td>"])
(def HEADER_PRODUCT_NAME ["<td colspan='3'>" :proddesc "</td>"])
(def HEADER_LINE_1_TERMINATOR ["</tr>\n"])

(def REQT_HEADER ["<tr class='requirement'><td class='linenum'>" :linenum "</td><td/><td/><td>" :reqtdesc "</td><td class='criteria'>" :score-key "</td><td>" :abs-weight "</td><td/><td/>"])
(def REQT_PRODUCT_SCORE ["<td>" :score "</td><td/><td>" :rel-score "</td>"])
(def REQT_TERMINATOR ["</tr>\n"])

(def SUBCATEGORY_HEADER ["<tr class='subcategory'><td class='linenum'>" :linenum "</td><td/><td colspan='3' class='subcategory-name'>" :subcategory "</td><td/><td class='subcategory-weight'>" :subcategory-weight "</td><td/>"])
(def SUBCATEGORY_PRODUCT_SCORE ["<td/><td/><td>" :score "</td>"])
(def SUBCATEGORY_TERMINATOR ["</tr>\n"])

(def CATEGORY_HEADER ["<tr class='category'><td class='linenum'>" :linenum "</td><td colspan='4' class='category-name'>" :category "</td><td/><td/><td class='category-weight'>" :category-weight "</td>"])
(def CATEGORY_PRODUCT_SCORE ["<td/><td/><td>" :score "</td>"])
(def CATEGORY_TERMINATOR ["</tr>\n"])

(defn weighted-scores
  "Joins the scores, products, and requirements data, adding the :rel-wt (relative weight)
   and :rel-score (relative score) values for each requirement and score respectively"
  [scores prods reqts]
  (let [total-weight (reduce + 0 (rels/col-seq reqts :wt))]
    (-> 
      (rels/append reqts (fn [r] {:rel-wt (/ (:wt r) total-weight)}))
      (rels/join scores [= :reqtid :reqtid])
      (rels/append (fn [rs] {:rel-score (* (:rel-wt rs) (:score rs))}))
      (rels/join prods [= :prodid :prodid]))))

(defn denormalize-scores
  "consolidates the relation so that there is one row per requirement,
   all scores for that requirement are aggregated into the :scores attribute"
  [r]
  (-> 
    (rels/denormalize r :scores :prodid :proddesc :score :rel-score)
    (rels/append (fn [_] {:type :requirement}))))

(defn aggregate-scores
  "computes a list of the aggregated scores for each product in the relation w where (pred %) is true"
  [pred w]
  (-> (rels/select w pred)
      ((comp flatten rels/col-seq) :scores)
      ; the contents of scores is a sequence of maps, which is itself a relation
      (rels/project [:prodid :rel-score])
      ; gather the values for one product
      (#(group-by %2 %1) :prodid)
      ; sum all the values for the product so we end up with a map with :prodid and :score (with a single value)
      ; TODO: need to make sure products are in the same order as elsewhere
      (#(map %2 %1) (fn [[k v]] {:score (reduce #(+ (:rel-score %2) %1) 0 v)}))))

(defn subcategory-scores
  [c s w]
  (aggregate-scores #(and (= (:category %) c) (= (:subcategory %) s)) w))

(defn category-scores
  [category w]
  (aggregate-scores #(= (:category %) category) w))

(defn conj-category-rows
  "Adds a row to the relation for each of the categories and subcategories associated with the requirements"
  [waverel]
  (let [cats (into #{} (rels/col-seq waverel :category))
        subcats (into #{} (rels/project waverel [:category :subcategory]))]
    (concat waverel
      ; TODO: need to roll up scores for each level here as well
      (map #(assoc {} :type :category 
                      :category % 
                      :subcategory "" 
                      :reqtdesc "" 
                      :scores (category-scores % waverel)) 
           cats)
      (map (fn [{c :category s :subcategory}] 
             (assoc {} 
                    :type :subcategory 
                    :category c 
                    :subcategory s 
                    :reqtdesc ""
                    :scores (subcategory-scores c s waverel))) 
           subcats))))
                    
(defn build-wave-relation
  "Combines the data in the specified wave, computes score values, and returns a consolidated relation
   for rendering the wave report"
  [{:keys [scores products requirements]}]
  (let [ds (-> (weighted-scores scores products requirements)
               (denormalize-scores)
               (conj-category-rows))]
    (sort-by #(vector (:category %) (:subcategory %) (:reqtdesc %)) ds)))


(defmulti render :type)
(defmethod render :category [c]
  (str (parent-child-template-map CATEGORY_HEADER CATEGORY_PRODUCT_SCORE :scores c) (template-map CATEGORY_TERMINATOR c)))
(defmethod render :subcategory [s]
  (str (parent-child-template-map SUBCATEGORY_HEADER SUBCATEGORY_PRODUCT_SCORE :scores s) (template-map SUBCATEGORY_TERMINATOR s)))
(defmethod render :requirement [reqt]
  (str (parent-child-template-map REQT_HEADER REQT_PRODUCT_SCORE :scores reqt) (template-map REQT_TERMINATOR reqt)))

(defn gen-html
  "returns a string containing the HTML representation of the wave"
  [wave]
  (apply str 
    ; TODO: header
    (map render (build-wave-relation wave))))
    ; TODO: footer

