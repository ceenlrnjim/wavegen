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
         (#(map-indexed (fn [ix r] (assoc r :rownum ix)) %1))
         ))                                                                  ; add rows at the beginning with the headers 

(defn merge-cols
  [row start end]
  (.addMergedRegion 
    (.getSheet row)
    (org.apache.poi.ss.util.CellRangeAddress.  (.getRowNum row) (.getRowNum row) start end)))

; TODO: support for styling (Center, colors, etc)
(defn make-cells
  [row ix & specs]
  (if (empty? specs) nil
    (let [[t v n] (first specs)
          c (.createCell row ix)]
      (if (= t :formula)
        (.setCellFormula c v))
        (.setCellValue c v)
      (when n (merge-cols row ix (+ ix (- n 1))))
      (apply make-cells row (if n (+ ix n) (inc ix)) (rest specs)))))

(defn ix-to-cell
  [r c]
  (str  (get "ABCDEFGHIJKLMNOPQRSTUVWXYZ" c) (+ 1 r)))

(defn cell-list
  [cs]
  (reduce #(str %1 "," %2) cs))

(defn ix-list
  [ixs]
  (cell-list (map #(apply ix-to-cell %) ixs)))

(defn cell-range
  [ca cb]
  (str ca ":" cb))

(defn ix-range
  [ra ca rb cb]
  (cell-range (ix-to-cell ra ca) (ix-to-cell rb cb)))
  

(defn subcat-rowids
  [r c]
  (into #{} 
    (-> (rels/select r #(and (= (:type %) :subcatgory) (= (:category %) c)))
      (rels/col-seq :rownum))))

(defn sum-rows-formula
  [r pred colix]
  (let [rows (into #{} (-> (rels/select r pred) (rels/col-seq :rownum)))
        cells (ix-list (map #(vector % colix) rows))]
    (str "SUM(" cells ")")))

(defn sum-subcats-formula
  [r cat colix]
  (sum-rows-formula r #(and (= (:type %) :subcategory) (= (:category %) cat)) colix))

(defn sum-reqts-formula
  [r c s colix]
  (sum-rows-formula r #(and (= (:type %) :requirement) (= (:category %) c) (= (:subcategory %) s)) colix))
   

;(defn reqt-range
  ;[r c s]
  ;(-> 
    ;(rels/select r #(and (= (:type %) :requirement) (= (:subcategory %) s) (= (:category %) c)))
    ;(rels/col-seq :rownum)
    ;((fn [s] [(min s) (max s)]))))


(defmulti add-cells (fn [ctx data] (:type data)))
(defmethod add-cells :requirement [ctx data]
  (make-cells (:row ctx) 3
    [:value (:reqtdesc data)]
    [:value (str (:score-key data))])
  (make-cells (:row ctx) 6 [:formula (str (ix-to-cell (.getRowNum (:row ctx)) 5) "/SUM(F1:F1000)")]))

(defmethod add-cells :header [ctx data]
  (let [ps (map #(vector :value (:proddesc %) 3) (:scores data))]
    (apply make-cells (:row ctx) 5 (cons [:value "Weighting" 3] ps))))
    

(defmethod add-cells :subheader [ctx data]
  (let [ps (mapcat (fn [_] [[:value "Score"][:value "Notes"][:value "Wgt Score"]]) (:scores data))]
    (apply make-cells (:row ctx) 1 
      (concat [[:value "Category"] [:value "Sub-Category"] [:value "Requirement"] [:value "Eval Criteria"] [:value "Raw"] [:value "Wtd"][:value "Sub-cat"][:value "Category"]] ps))))

(defmethod add-cells :category [ctx data]
  (make-cells (:row ctx) 1 [:value (:category data) 4])
  (make-cells (:row ctx) 8  [:formula (sum-subcats-formula (:rel ctx) (:category data) 7)]))

(defmethod add-cells :subcategory [ctx data]
  (make-cells (:row ctx) 2 [:value (:subcategory data) 3])
  (make-cells (:row ctx) 7  [:formula (sum-reqts-formula (:rel ctx) (:category data) (:subcategory data) 6)]))

(defn gen-xlsx
  [wave]
  (let [wb (org.apache.poi.xssf.usermodel.XSSFWorkbook.)
        sheet (.createSheet wb)
        waverel (build-wave-relation wave)
        rowcounter (atom 0)]
    (doseq [data waverel]
      (let [row (.createRow sheet @rowcounter)]
          (add-cells {:book wb :sheet sheet :row row :rel waverel} data))
      (swap! rowcounter + 1))
    wb))
    
