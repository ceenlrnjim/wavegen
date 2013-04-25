(ns wavegen.excel
  (:import [org.apache.poi.ss.usermodel CellStyle])
  (:import [org.apache.poi.xssf.usermodel XSSFColor])
  (:import [java.awt Color])
  (:require [rels]))

(def styles (atom {}))

(defn ix-to-col
  [c]
  (str  (get "ABCDEFGHIJKLMNOPQRSTUVWXYZ" c)))

(defn ix-to-row
  [ix]
  (inc ix))

(defn relwt-formula
  [col]
  (fn [wave data col-ixs]
    (let [cltr (ix-to-col (get col-ixs col))
          total (str "SUM(" cltr "1:" cltr "1000)")]
      (str cltr (ix-to-row (:rownum data)) "/" total))))

(defn row-ids
  [r pred]
  (into #{} (-> (rels/select r pred) (rels/col-seq :rownum))))

(defn sum-subcat-formula
  [col]
  (fn [wave data col-ixs]
    (let [cltr (ix-to-col (get col-ixs col))
          rowids (row-ids wave #(and (= (:type %) :subcategory) (= (:category %) (:category data))))
          cells (reduce #(str %1 "," %2) (map #(str cltr (ix-to-row %)) rowids))]
      (str "SUM(" cells ")"))))

(defn sum-reqt-formula
  [col]
  (fn [wave data col-ixs]
    (let [cltr (ix-to-col (get col-ixs col))
          rowids (row-ids wave #(and (= (:type %) :requirement) (= (:category %) (:category data)) (= (:subcategory %) (:subcategory data))))
          begid (ix-to-row (apply min rowids))
          endid (ix-to-row (apply max rowids))]
      (str "SUM(" cltr begid ":" cltr endid ")"))))


(defn infix-binary-formula
  [op arg1 arg2]
  (fn [wave data col-ixs]
    (let [r (ix-to-row (:rownum data))
          c1 (ix-to-col (get col-ixs arg1))
          c2 (ix-to-col (get col-ixs arg2))]
    (str c1 r op c2 r))))


(def shared-columns [:rownum :cat :subcat :reqt :crit :raw :wtd :sub-wtd :cat-wtd])
(def product-specific-columns [:score :notes :score-wtd])

(def specs
  {:requirement 
    { :reqt [:reqtdesc {:style :reqt}]
      :crit [:score-key {:style :reqt}]
      :wtd  [(relwt-formula :raw)]
      :products 
        { :score-wtd [(infix-binary-formula "*" :score :wtd)] }}
   :category
    {:cat [:category {:mergecnt 4 :style :category-label}]
     :cat-wtd [(sum-subcat-formula :sub-wtd) {:style :category-value}] ;
     :rownum ["" {:style :category}]
     :raw ["" {:style :category}]
     :wtd ["" {:style :category}]
     :sub-wtd ["" {:style :category}]
     :products 
      {:score-wtd [(sum-subcat-formula :score-wtd) {:style :category-value}]
        :notes ["" {:style :category}]
        :score ["" {:style :category}]
      }}
   :subcategory
    {:subcat [:subcategory {:mergecnt 3 :style :subcategory-label}]
     :sub-wtd [(sum-reqt-formula :wtd) {:style :subcategory-value}]
     :rownum ["" {:style :subcategory-value}]
     :cat ["" {:style :subcategory-value}]
     :raw ["" {:style :subcategory-value}]
     :wtd ["" {:style :subcategory-value}]
     :cat-wtd ["" {:style :subcategory-value}]
     :products
      { :score-wtd [(sum-reqt-formula :score-wtd) {:style :subcategory-value}]
        :notes ["" {:style :subcategory-value}]
        :score ["" {:style :subcategory-value}]
      }}
      ; TODO: totals
   :header
     { :rownum ["" {:mergecnt 5 :style :header}]
       :raw ["Weightings" {:mergecnt 4 :style :header}]
       :products { :score [:proddesc {:mergecnt 3 :style :header}] }}
   :subheader
     { :cat ["Category" {:style :header}]
       :rownum ["" {:style :header}]
       :subcat ["Sub-Category" {:style :header}]
       :reqt ["Requirement" {:style :header}]
       :crit ["Evaluation Criteria" {:style :header}]
       :raw ["Abs Wt" {:style :header}]
       :wtd ["Rel Wt" {:style :header}]
       :sub-wtd ["Sub-cat" {:style :header}]
       :cat-wtd ["Category" {:style :header}]
       :products { :score ["Score" {:style :header}] 
                   :notes ["Notes" {:style :header}]
                   :score-wtd ["Wtd Score" {:style :header}] }}
      })


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

(defn pretty-map
  [m]
  (let [[f & r] (map (fn [[k v]] (str k " - " v)) m)]
    (reduce #(str %1 "\n" %2) f r)))


(defn build-wave-relation
  "Combines the data in the specified wave, computes score values, and returns a consolidated relation
   for rendering the wave report"
  [{:keys [products requirements]}]
     (-> (rels/append requirements (fn [t] {:type :requirement :score-key (pretty-map (:score-key t))}))
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

(defn make-cell
  [row cix {:keys [mergecnt style]}]
  (let [c (.createCell row cix)]
    ; TODO: other style support
    (when mergecnt (merge-cols row cix (+ cix (- mergecnt 1))))
    (when style
      (let [cellstyle (get @styles style)]
        (.setCellStyle c cellstyle)))
    c))

(defn set-row-contents
  [row spec col-ixs wave data]
  (doseq [[k [v styles]] spec]
    (let [cell (make-cell row (get col-ixs k) styles)]
      (cond (fn? v) (.setCellFormula cell (v wave data col-ixs))
            (keyword? v) (.setCellValue cell (get data v))
            :else (.setCellValue cell v)))))

(defn prod-col-ixs
  [prod-num prod-cols other-cols]
  (apply hash-map
    (flatten
      (map-indexed 
        (fn [ix c]
          [c (+ ix (count other-cols) (* (count prod-cols) prod-num))])
      prod-cols))))

(defn build-row
  [cols product-cols data {:keys [products wave row sheet]}]
  (let [col-ixs (apply hash-map (flatten (map-indexed (comp reverse vector) cols)))
        spec (get specs (:type data))
        prod-spec (:products spec)
        row-spec (dissoc spec :products)]
    (when spec
      (set-row-contents row row-spec col-ixs wave data)
      (doseq [[pi p] (map-indexed vector products)]
        (set-row-contents row prod-spec 
          (merge (prod-col-ixs pi product-cols cols) col-ixs) 
          wave 
          (merge p data))))))

(defn init-styles
  [wb]
  (swap! styles merge 
  {:header (doto (.createCellStyle wb)
                  (.setAlignment CellStyle/ALIGN_CENTER)
                  (.setFillPattern CellStyle/SOLID_FOREGROUND)
                  (.setWrapText true)
                  (.setFillForegroundColor (XSSFColor. (Color. (/ 141.0 256) (/ 180.0 256) (/ 226.0 256)))))
    :category (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (XSSFColor. (Color. (/ 197.0 256) (/ 217.0 256) (/ 241.0 256)))))
    :category-label (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (XSSFColor. (Color. (/ 197.0 256) (/ 217.0 256) (/ 241.0 256)))))
    :category-value (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (XSSFColor. (Color. (/ 197.0 256) (/ 217.0 256) (/ 241.0 256))))
              (.setAlignment CellStyle/ALIGN_CENTER))
    :subcategory-label (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (XSSFColor. (Color. (/ 235.0 256) (/ 241.0 256) (/ 222.0 256)))))
    :subcategory-value (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (XSSFColor. (Color. (/ 235.0 256) (/ 241.0 256) (/ 222.0 256))))
              (.setAlignment CellStyle/ALIGN_CENTER))
    :reqt (doto (.createCellStyle wb) (.setWrapText true))
  }))
                  


(defn gen-xlsx
  [wave]
  (let [wb (org.apache.poi.xssf.usermodel.XSSFWorkbook.)
        sheet (.createSheet wb)
        waverel (build-wave-relation wave)
        styles (init-styles wb)
        rowcounter (atom 0)]
    (doseq [data waverel]
      (let [row (.createRow sheet @rowcounter)]
          (build-row 
            shared-columns 
            product-specific-columns 
            data 
            {:products (:products wave) :book wb :sheet sheet :row row :wave waverel}))
      (swap! rowcounter + 1))
    (.setColumnWidth sheet 0 (* 256 2))
    (.setColumnWidth sheet 1 (* 256 10))
    (.setColumnWidth sheet 2 (* 256 10))
    (.setColumnWidth sheet 3 (* 256 50))
    (.setColumnWidth sheet 4 (* 256 45))
    (.setColumnWidth sheet 5 (* 256 8))
    (.setColumnWidth sheet 6 (* 256 8))
    (.setColumnWidth sheet 7 (* 256 8))
    (.setColumnWidth sheet 8 (* 256 9))
    (doseq [i (range 9 (+ 9 (count (:products wave))))]
      (.setColumnWidth sheet i (* 256 8)))
    wb))
