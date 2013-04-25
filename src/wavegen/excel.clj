(ns wavegen.excel
  (:import [org.apache.poi.ss.usermodel CellStyle IndexedColors Font])
  (:import [org.apache.poi.xssf.usermodel XSSFColor])
  (:import [java.awt Color])
  (:require [rels]))

(def styles (atom {}))

(defn color [r g b] (XSSFColor. (Color. (float (/ r 256.0)) (/ g 256.0) (/ b 256.0))))
(defn ix2c [c] (str  (get "ABCDEFGHIJKLMNOPQRSTUVWXYZ" c)))
(defn ix2r [ix] (inc ix))
(defn ix-seq [s] (map-indexed vector s))

(defn relwt-formula
  [col]
  (fn [wave {rownum :rownum} col-ixs]
    (let [cltr (ix2c (get col-ixs col))]
      (str cltr (ix2r rownum) "/SUM(" cltr "1:" cltr "1000)"))))

(defn row-ids
  [r pred]
  (into #{} (-> (rels/select r pred) (rels/col-seq :rownum))))

(defn type-pred
  [t data & ks]
  (fn [r]
    (and (= (:type r) t)
      (every? (fn [k] (= (get r k) (get data k))) ks))))

(defn items-formula
  [col op rowtype & kvs]
  (fn [wave data col-ixs]
    (let [cltr (ix2c (get col-ixs col))
          rowids (row-ids wave (apply type-pred rowtype data kvs))
          cells (reduce #(str %1 "," %2) (map #(str cltr (ix2r %)) rowids))]
      (str op "(" cells ")"))))

(defn sum-subcat-formula
  [col]
  (items-formula col "SUM" :subcategory :category))

(defn sum-cat-formula
  [col]
  (items-formula col "SUM" :category))

(defn sum-reqt-formula
  [col]
  (items-formula col "SUM" :requirement :subcategory :category))


(defn infix-binary-formula
  [op arg1 arg2]
  (fn [wave data col-ixs]
    (let [r (ix2r (:rownum data))
          c1 (ix2c (get col-ixs arg1))
          c2 (ix2c (get col-ixs arg2))]
    (str c1 r op c2 r))))


(def shared-columns [:rownum :cat :subcat :reqt :crit :raw :wtd :sub-wtd :cat-wtd])
(def product-specific-columns [:score :notes :score-wtd])

; TODO support taking scores from the relation if they're specified?
(def specs
  {:requirement 
    { :reqt [:reqtdesc {:style :reqt}]
      :crit [:score-key {:style :reqt}]
      :wtd  [(relwt-formula :raw)]
      :cat-wtd ["" {:style :reqt :border :right}]
      :products 
        { :score-wtd [(infix-binary-formula "*" :score :wtd) { :style :reqt :border :right}] }}
   :category
    {:cat [:category {:mergecnt 4 :style :category-label}]
     :cat-wtd [(sum-subcat-formula :sub-wtd) {:style :category-value :border :right}] ;
     :rownum ["" {:style :category}]
     :raw ["" {:style :category}]
     :wtd ["" {:style :category}]
     :sub-wtd ["" {:style :category}]
     :products 
      {:score-wtd [(sum-subcat-formula :score-wtd) {:style :category-value :border :right}]
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
     :cat-wtd ["" {:style :subcategory-value :border :right}]
     :products
      { :score-wtd [(sum-reqt-formula :score-wtd) {:style :subcategory-value :border :right}]
        :notes ["" {:style :subcategory-value}]
        :score ["" {:style :subcategory-value}]
      }}
   :totals 
    {:rownum ["Final Score:" {:style :totals :mergecnt 9 :border :top}]
     :products 
      { :score [(sum-cat-formula :score-wtd) {:mergecnt 3 :style :totals :border :right }] }}
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


; styles are functions so that each cell gets its own instance so that applying borders only impacts that single cell
(defn init-styles
  [wb]
  (swap! styles merge 
  {:header (fn [] (doto (.createCellStyle wb)
                  (.setAlignment CellStyle/ALIGN_CENTER)
                  (.setFillPattern CellStyle/SOLID_FOREGROUND)
                  (.setWrapText true)
                  (.setFillForegroundColor (color 141.0 180.0 226.0 ))))
    :category (fn [] (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (color 197.0 217.0 241.0))))
    :category-label (fn [] (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (color 197.0 217.0 241.0))))
    :category-value (fn [] (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (color 197.0 217.0 241.0))
              (.setAlignment CellStyle/ALIGN_CENTER)))
    :subcategory-label (fn [] (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (color 235.0 241.0 222.0))))
    :subcategory-value (fn [] (doto (.createCellStyle wb)
              (.setFillPattern CellStyle/SOLID_FOREGROUND)
              (.setFillForegroundColor (color 235.0 241.0 222.0))
              (.setAlignment CellStyle/ALIGN_CENTER)))
    :totals (fn [] (doto (.createCellStyle wb)
              (.setAlignment CellStyle/ALIGN_RIGHT)
              (.setFont (doto (.createFont wb) (.setBoldweight Font/BOLDWEIGHT_BOLD)))))
    :reqt (fn [] (doto (.createCellStyle wb) (.setWrapText true)))
  }))
                  


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
         (rels/denormalize :scores :prodid :proddesc)
         (conj-category-rows)
         (#(sort-by %2 %1) #(vector (:category %) (:subcategory %) (:reqtdesc %)))
         (#(concat %1 [{:type :totals :scores products}])); add line for totals
         (#(concat [{:type :header :scores products} {:type :subheader :scores products}] %1))
         (#(map-indexed (fn [ix r] (assoc r :rownum ix)) %1))))

(defn merge-cols
  [row start end]
  (.addMergedRegion 
    (.getSheet row)
    (org.apache.poi.ss.util.CellRangeAddress.  (.getRowNum row) (.getRowNum row) start end)))

(defn set-border
  [cs side]
  (let [bs CellStyle/BORDER_THIN
        bc (.getIndex IndexedColors/BLACK)
        sides (if (set? side) side (hash-set side))]
    (doseq [s sides]
      (condp = s
        :top (doto cs (.setBorderTop bs) (.setTopBorderColor bc))
        :bottom (doto cs (.setBorderBottom bs) (.setBottomBorderColor bc))
        :left (doto cs (.setBorderLeft bs) (.setLeftBorderColor bc))
        :right (doto cs (.setBorderRight bs) (.setRightBorderColor bc))))))

(defn make-cell
  [row cix {:keys [mergecnt style border]}]
  (let [c (.createCell row cix)]
    (when mergecnt (merge-cols row cix (+ cix (- mergecnt 1))))
    (when style
      (let [cellstyle ((get @styles style))]
        (when border (set-border cellstyle border))
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
  (into {}
    (map-indexed 
      (fn [ix c] [c (+ ix (count other-cols) (* (count prod-cols) prod-num))])
    prod-cols)))

(defn build-row
  [cols product-cols data {:keys [products wave row sheet]}]
  (let [col-ixs (into {} (map-indexed (comp vec reverse vector) cols))
        spec (get specs (:type data))
        prod-spec (:products spec)
        row-spec (dissoc spec :products)]
    (when spec
      (set-row-contents row row-spec col-ixs wave data)
      (doseq [[pi p] (ix-seq products)]
        (set-row-contents row prod-spec 
          (merge (prod-col-ixs pi product-cols cols) col-ixs) 
          wave 
          (merge p data))))))


(defn gen-xlsx
  "Given a wave data structure, generate an xls workbook that computes scores and weights"
  [wave]
  (let [wb (org.apache.poi.xssf.usermodel.XSSFWorkbook.)
        sheet (.createSheet wb)
        waverel (build-wave-relation wave)]
    (init-styles wb)
    (doseq [[rowcounter data] (ix-seq waverel)]
      (let [row (.createRow sheet rowcounter)]
        (build-row shared-columns product-specific-columns data 
          {:products (:products wave) :book wb :sheet sheet :row row :wave waverel})))
    (doseq [[i c] (ix-seq [2 10 10 50 45 8 8 8 9])]
      (.setColumnWidth sheet i (* 256 c)))
    (doseq [i (range 9 (+ 9 (count (:products wave))))]
      (.setColumnWidth sheet i (* 256 8)))
    wb))
