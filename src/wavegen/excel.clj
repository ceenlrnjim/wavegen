(ns wavegen.excel
  (:require [rels]))

(defn conj-category-rows
  "Adds a row to the relation for each of the categories and subcategories associated with the requirements"
  [waverel]
  (let [cats (into #{} (rels/col-seq waverel :category))
        subcats (into #{} (rels/project waverel [:category :subcategory]))]
    (concat waverel
      (map (fn [c] {:type :category :category c }) cats)
      (map (fn [{c :category s :subcategory}] 
             {:type :subcategory :category c :subcategory s })
           subcats))))

(defn cons-headers
  "Adds header rows to the specified wave relation"
  [w products]
  (concat
    [{:type :header :scores products} ; using scores to fit in render function
     {:type :subheader :scores products}]
    w))

(defn build-wave-relation
  "Combines the data in the specified wave, computes score values, and returns a consolidated relation
   for rendering the wave report"
  [{:keys [products requirements]}]
     (-> (rels/append requirements (fn [_] {:type :requirement}))
         (rels/join products) ; cartesian - no scores
         (rels/denormalize :scores :prodid :proddesc)                                               ; denormailize to one row per requirement
         (conj-category-rows)                                                                       ; add rows for categories and subcategories
         (#(sort-by %2 %1) #(vector (:category %) (:subcategory %) (:reqtdesc %)))                  ; sort the rows into the right order
         ;(conj-totals)                                                                              ; add row for totals at the end
         (cons-headers products)
         ))                                                                  ; add rows at the beginning with the headers 

(defn merge-cols
  [row start end]
  (.addMergedRegion 
    (.getSheet row)
    (org.apache.poi.ss.util.CellRangeAddress.  (.getRowNum row) (.getRowNum row) start end)))

; TODO: support for styling (Center, colors, etc)
(defn cells
  [row ix & specs]
  (if (empty? specs) nil
    (let [[v n] (first specs)
          c (.createCell row ix)]
      (.setCellValue c v)
      (when n (merge-cols row ix (+ ix (- n 1))))
      (apply cells row (if n (+ ix n) (inc ix)) (rest specs)))))


(defmulti add-cells (fn [ctx data] (:type data)))
(defmethod add-cells :requirement [ctx data]
  (cells (:row ctx) 3
    [(:reqtdesc data)]
    [(str (:score-key data)) 3]))

(defmethod add-cells :header [ctx data]
  (let [ps (map #(vector (:proddesc %) 3) (:scores data))]
    (apply cells (:row ctx) 5 (cons ["Weighting" 3] ps))))
    

(defmethod add-cells :subheader [ctx data]
  (let [ps (mapcat (fn [_] [["Score"]["Notes"]["Wgt Score"]]) (:scores data))]
    (apply cells (:row ctx) 1 
      (concat [["Category"] ["Sub-Category"] ["Requirement"] ["Eval Criteria"] ["Reqt"]["Sub-cat"]["Category"]] ps))))

(defmethod add-cells :category [ctx data]
)
(defmethod add-cells :subcategory [ctx data]
)

(defn gen-xlsx
  [wave]
  (let [wb (org.apache.poi.xssf.usermodel.XSSFWorkbook.)
        sheet (.createSheet wb)
        waverel (build-wave-relation wave)
        rowcounter (atom 1)]
    (doseq [data waverel]
      (let [row (.createRow sheet @rowcounter)]
          (add-cells {:book wb :sheet sheet :row row} data))
      (swap! rowcounter + 1))
    wb))
    
