(ns wavegen.html
  (:use [wavegen.core])
  (:use [wavegen.aggr]))

(def formatter (java.text.DecimalFormat. "#0.0#"))
(defn decfmt 
  "Returns the specific number as a standard format string"
  [d] (.format formatter d))

; TODO: move HTML snippets to resource file and use reader to load separate from code
(def HEADER_LINE_1_HEADER "<tr class='header'><td colspan='5'></td><td colspan='3'>Weightings</td>")
(def HEADER_PRODUCT_NAME "<td colspan='3'>~NAME~</td>")
(def HEADER_LINE_1_TERMINATOR "</tr>\n")

(def HEADER_LINE_2_HEADER "<tr class='header'><td class='linenumlabel'></td><td class='categorylabel'>Category</td><td class='subcategorylabel'>Sub-category</td><td class='reqtlabel'>Requirement</td><td class='criterialabel'>Evaluation Criteria</td><td class='reqtwtlabel'>Reqt</td><td class='subcatwtlabel'>Sub-cat</td><td class='catwtlabel'>Category</td>")
(def HEADER_LINE_2_PRODUCT_HEADER "<td class='scorelabel'>Score</td><td class='noteslabel'>Notes</td><td class='wtscorelabel'>Wgt Score</td>")
(def HEADER_LINE_2_TERMINATOR "</tr>\n")

(def CATEGORY_HEADER "<tr class='category'><td class='linenum'>~LINE~</td><td colspan='4' class='category-name'>~NAME~</td><td/><td/><td class='category-weight'>~WEIGHT~</td>")
(def CATEGORY_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
(def CATEGORY_TERMINATOR "</tr>\n")

(def SUBCATEGORY_HEADER "<tr class='subcategory'><td class='linenum'>~LINE~</td><td/><td colspan='3' class='subcategory-name'>~NAME~</td><td/><td class='subcategory-weight'>~WEIGHT~</td><td/>")
(def SUBCATEGORY_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
(def SUBCATEGORY_TERMINATOR "</tr>\n")

(def REQT_HEADER "<tr class='requirement'><td class='linenum'>~LINE~</td><td/><td/><td>~:reqtdesc~</td><td class='criteria'>~:scores~</td><td>~:abs-weight~</td><td/><td/>")
(def REQT_PRODUCT_SCORE "<td>~:raw~</td><td/><td>~:score~</td>")
(def REQT_TERMINATOR "</tr>\n")

(def TOTAL_HEADER "<tr class='total'><td/><td/><td/><td/><td colspan='4'>Final Score:</td>")
(def TOTAL_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
(def TOTAL_TERMINATOR "</tr>\n")

(defn denormalize-scores
  [scores prods reqts]
  (-> 
    ; TODO: need to figure out what's wrong with hash join - not commutative
    (rels/nested-loop-join scores prods [:prodid :prodid = ])
    (rels/nested-loop-join reqts [:reqtid :reqtid =])
    (rels/denormalize :scores :prodid :proddesc :score)))

(defn build-wave-relation
  [{:keys [scores products requirements]}]
  (let [ds (denormalize-scores scores products requirements)]
    (sort-by #(vector (:category %) (:subcategory %) (:reqtdesc %)) ds)))


; TODO: expand to support replacing repeating key values with some repeating substring
(defn template-map
  "takes a template with delimited placeholders and fills the placeholders with the values in the map"
  [template m]
  (reduce #(.replace % (str "~" %2 "~") (str (get m %2))) template (keys m)))

(defn reqt-line
  "Converts a single requirement line into corresponding html"
  [reqt]
  (str (template-map REQT_HEADER reqt) 
       (reduce str "" (map #(template-map REQT_PRODUCT_SCORE %) (:scores reqt)))
       (template-map REQT_TERMINATOR reqt)))

(defn gen-html
  "returns a string containing the HTML representation of the wave"
  [wave]
  (apply str (map reqt-line (build-wave-relation wave))))
  ;(doseq [{:keys [category subcategory reqtdesc]} (build-wave-relation wave)] 
    ;(println category " -> " subcategory " -> " reqtdesc))
  ;nil)

