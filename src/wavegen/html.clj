(ns wavegen.html
  (:use [wavegen.core])
  (:use [wavegen.aggr]))

(def formatter (java.text.DecimalFormat. "#0.0#"))

; TODO: move HTML snippets to resource file and use reader to load separate from code
;
; HTML generation functions ------------------------------------------------------------------------
;
(defn- replace-tokens
  "Replaces each of the tokens in the token/value pairs with the corresponding value in the specified string"
  [string & token-value-pairs]
  (if (empty? token-value-pairs) string
    (recur (.replace string (str (first token-value-pairs)) (str (second token-value-pairs))) (rest (rest token-value-pairs)))))

(defn- page
  "renders the page level html"
  [output wave]
  (.append output "<html><head><link rel='stylesheet' href='wave.css'></head><body><table>"))

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
  (.append output (replace-tokens CATEGORY_HEADER "~NAME~" cat "~WEIGHT~" (.format formatter weight)))
  (doseq [pid prod-ids]
    (.append output (replace-tokens CATEGORY_PRODUCT_SCORE "~SCORE~" (.format formatter (get scores pid)))))
  (.append output CATEGORY_TERMINATOR))


(def SUBCATEGORY_HEADER "<tr class='subcategory'><td/><td/><td colspan='3' class='subcategory-name'>~NAME~</td><td/><td class='subcategory-weight'>~WEIGHT~</td><td/>")
(def SUBCATEGORY_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
(def SUBCATEGORY_TERMINATOR "</tr>")
; TODO: very similar to category - refactor
(defn- subcategory
  [output cat sub weight scores prod-ids]
  (.append output (replace-tokens SUBCATEGORY_HEADER "~NAME~" sub "~WEIGHT~" (.format formatter weight)))
  (doseq [pid prod-ids]
    (.append output (replace-tokens SUBCATEGORY_PRODUCT_SCORE "~SCORE~" (.format formatter (get scores pid)))))
  (.append output SUBCATEGORY_TERMINATOR))

(def REQT_HEADER "<tr class='requirement'><td/><td/><td/><td>~DESC~</td><td class='criteria'>~CRITERIA~</td><td>~WEIGHT~</td><td/><td/>")
(def REQT_PRODUCT_SCORE "<td>~RAW~</td><td/><td>~SCORE~</td>")
(def REQT_TERMINATOR "</tr>")
(defn- requirement
  [output wave r prod-ids]
  (.append output (replace-tokens REQT_HEADER "~DESC~" (:desc r) "~CRITERIA~" (:scores r) "~WEIGHT~" (:wt r)))
  (doseq [pid prod-ids]
    (.append output (replace-tokens REQT_PRODUCT_SCORE "~RAW~" (get-score wave pid (:id r)) "~SCORE~" (.format formatter (weighted-score wave (:id r) pid)))))
  (.append output REQT_TERMINATOR))

(def TOTAL_HEADER "<tr class='total'><td/><td/><td/><td/><td colspan='4'>Final Score:</td>")
(def TOTAL_PRODUCT_SCORE "<td/><td/><td>~SCORE~</td>")
(def TOTAL_TERMINATOR "</tr>")

(defn- totals
  [output scores prod-ids]
  (.append output TOTAL_HEADER)
  (doseq [pid prod-ids]
    (.append output (replace-tokens TOTAL_PRODUCT_SCORE "~SCORE~" (.format formatter (get scores pid)))))
  (.append output TOTAL_TERMINATOR))

(defn- end-page
  [output wave]
  (.append output "</table></body></html>"))

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
        (subcategory output c sc (subcat-weight wave c sc) (subcat-scores wave c sc prodlist) prodlist)
        (doseq [r (requirements wave c sc)]
          (requirement output wave r prodlist))))
    (totals output (total-scores wave prodlist) prodlist)
    (end-page output wave)
    (.toString output)))

