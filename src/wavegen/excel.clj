(ns wavegen.excel
  (:import [org.apache.poi.ss.usermodel Workbook CellStyle])
  (:import [org.apache.poi.hssf.usermodel HSSFWorkbook HSSFFont])
  (:import [org.apache.poi.hssf.util HSSFColor HSSFColor$WHITE HSSFColor$DARK_BLUE HSSFColor$BLUE HSSFColor$LIGHT_BLUE])
  (:import [org.apache.poi.ss.util CellRangeAddress])
  (:use [wavegen.core])
  (:use [wavegen.aggr]))

(def ^:dynamic *row-counter* (atom -1))

(defn- nextrowid
  "Returns next row id as an int"
  []
  (int (swap! *row-counter* inc)))

;
; Styling functions -------------------------------------------------------------------------
;
(def cellstyles (atom {}))

; TODO: add style details
(defn- header-style
  [wb font]
  (doto (.createCellStyle wb)
    (.setFillForegroundColor HSSFColor$DARK_BLUE/index)
    ;(.setFillBackgroundColor HSSFColor$DARK_BLUE/index)
    (.setFillPattern CellStyle/SOLID_FOREGROUND)
    (.setFont font)
    (.setAlignment CellStyle/ALIGN_CENTER)))

(defn- reqt-style
  [wb]
  (doto (.createCellStyle wb)
    (.setWrapText true)
    (.setAlignment CellStyle/ALIGN_LEFT)))

(defn- subcat-style
  [wb font]
  (doto (.createCellStyle wb)
    (.setFillForegroundColor HSSFColor$LIGHT_BLUE/index)
    (.setFillBackgroundColor HSSFColor$DARK_BLUE/index)
    (.setFillPattern CellStyle/SOLID_FOREGROUND)
    (.setFont font)
    (.setAlignment CellStyle/ALIGN_LEFT)))

(defn- cat-style
  [wb font]
  (doto (.createCellStyle wb)
    (.setFillForegroundColor HSSFColor$BLUE/index)
    (.setFillBackgroundColor HSSFColor$DARK_BLUE/index)
    (.setFillPattern CellStyle/SOLID_FOREGROUND)
    (.setFont font)
    (.setAlignment CellStyle/ALIGN_LEFT)))

(defn- total-style
  [wb]
  (doto (.createCellStyle wb)
    (.setAlignment CellStyle/ALIGN_LEFT)))

(defn- init-styles
  [wb]
  (let [headerfont (.createFont wb)]
    (doto headerfont
      (.setColor HSSFColor$WHITE/index))
    (swap! cellstyles assoc :header (header-style wb headerfont))
    (swap! cellstyles assoc :subcategory (subcat-style wb headerfont))
    (swap! cellstyles assoc :category (cat-style wb headerfont))
    (swap! cellstyles assoc :totals (total-style wb))
    (swap! cellstyles assoc :requirement (reqt-style wb))))


;
; Utility functions -------------------------------------------------------------------------
;

(defn- addcell
  ([row cellix value stylekey valfunc]
    (let [cell (.createCell row cellix)]
      ;(println "Setting cell value " value " of type " (.getClass value))
      (valfunc cell value)
      (.setCellStyle cell (get @cellstyles stylekey))
      cell))
  ; Adds a row that spans the specified cell indices
  ([row cellstartix cellendix value stylekey valfunc]
    (let [cell (addcell row cellstartix value stylekey valfunc)]
      (.addMergedRegion (.getSheet row) (CellRangeAddress. (.getRowNum row) (.getRowNum row) cellstartix cellendix))
      cell)))

(defn- add-value-cell
  "Adds a cell with the specified value and style key to the specified sheet.
  Style key is used as the key to the cellstyles map.  returns the cell"
  ([row cellix value stylekey]
    (addcell row cellix value stylekey #(.setCellValue %1 %2)))
  ; Adds a row that spans the specified cell indices
  ([row cellstartix cellendix value stylekey]
    (addcell row cellstartix cellendix value stylekey #(.setCellValue %1 %2))))

(defn- add-formula-cell
  "Adds a cell with the specified value and style key to the specified sheet.
  Style key is used as the key to the cellstyles map.  returns the cell"
  ([row cellix formula stylekey]
    (addcell row cellix formula stylekey #(.setCellFormula %1 %2)))
  ; Adds a row that spans the specified cell indices
  ([row cellstartix cellendix formula stylekey]
    (addcell row cellstartix cellendix formula stylekey #(.setCellFormula %1 %2))))

(defn- add-value-cells
  "Adds a range of values of a specified width onto the end of a row"
  ([row startix values style]
    (let [counter (atom -1)]
      (doseq [v values]
        (add-value-cell row (+ startix (swap! counter inc)) v style))))
  ([row startix width values style]
    (let [counter (atom (- startix width))]
      (doseq [v values]
        (let [ix1 (swap! counter + width)
              ix2 (+ ix1 width -1)]
          ;(println "Adding cell" v "from" ix1 "to" ix2)
          (add-value-cell row ix1 ix2 v style))))))

(defn- itemixseq
  "Returns a sequence of tuples that contain item from specified seq at second location and index 
  in the sequence as the first item"
  [s]
  (map-indexed #(vector %1 %2) s))

(defn- range-formula
  "Takes a zero indexed collection of rows numbers (cells for poi api) and returns a  string with a sum range formula (1 indexed for the sheet)"
  [formula column-letter rows]
  (str formula "($" column-letter (+ 1 (apply min rows)) ":$" column-letter (+ 1 (apply max rows)) ")"))

(defn- list-formula
  "Returns the formula over the specified list of 0-indexed row numbers"
  [formula column-letter rows]
  (str 
    (reduce
      #(str %1 "," %2)
      (str formula "(")
      (map #(str "$" column-letter (+ 1 %)) rows))
    ")"))

(def letter-lookup (reduce #(assoc %1 (first %2) (second %2)) {} (map-indexed #(vector %1 %2) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defn- col-letter
  "Returns the letter designation for the specified 0-indexed column number"
  [i]
  (get letter-lookup i))


;
; Generation functions -------------------------------------------------------------------------
;


(defn- header
  "writes the header rows to the specified sheet for the specified wave"
  [sheet wave]
  (let [header1rowix (nextrowid)
        header2rowix (nextrowid)
        header1row (.createRow sheet header1rowix)
        header2row (.createRow sheet header2rowix)]
    (add-value-cell header1row 0 3 "" :header)
    (add-value-cell header1row 4 6 "Weightings" :header)
    (add-value-cells header1row 7 3 (map #(product-desc wave %) (prod-keys wave)) :header)
    (add-value-cells header2row 0 ["Category" "Sub-category" "Requirement" "Evaluation Criteria" "Reqt" "Sub-cat" "Category"] :header)
    (add-value-cells header2row 7 (flatten (repeat (count (prod-keys wave)) ["Score" "Notes" "Wgt Score"])) :header)))

(defn- scores-str
  "Returns a string representing the scores meaning map"
  [m]
  (reduce
    #(str %1 "\n" (first %2) ":" (second %2))
    ""
    m))

(defn- requirement
  "adds row and cells for the specified requirement.
  Returns the row number for this requirement."
  [sheet wave cat subcat r]
  (let [reqtrow (.createRow sheet (nextrowid))]
    (add-value-cells reqtrow 0 [""  ; cat
                         ""  ; subcat
                         (:desc r) ; requirement
                         (scores-str (:scores r)) ; score meaning
                         (double (reqt-weight wave r)) ; requirement weight
                         ""  ;sub-cat weight
                         ""] ; cat-weight
                         :requirement)
    (doseq [pid (itemixseq (prod-keys wave))]
      (add-value-cells reqtrow (+ 7 (* 3 (first pid))) 
                        [(double (get-score wave (second pid) (:id r))) ;raw score
                         "" ; notes
                         (double (weighted-score wave (:id r) (second pid)))] ;weighted score
                         :requirement))
    (.getRowNum reqtrow)))


(defn- subcategory
  "Adds a row and cells for the specified subcategory over the specified row ids - returns the index for this row"
  [subcat sheet subcatrowix wave reqt-row-nums]
  (let [subcatrow (.createRow sheet subcatrowix)]
    (add-value-cell subcatrow 0 "" :subcategory)
    (add-value-cell subcatrow 1 3 subcat :subcategory)
    (add-value-cell subcatrow 4 "" :subcategory)
    (add-formula-cell subcatrow 5 (range-formula "sum" "e" reqt-row-nums) :subcategory)
    (add-value-cell subcatrow 6 "" :subcategory)
    (doseq [pid (itemixseq (prod-keys wave))]
      (let [valcolix (+ 9 (* 3 (first pid)))]
        (add-value-cells subcatrow (- valcolix 2) ["" ""] :subcategory)
        (add-formula-cell subcatrow valcolix (range-formula "sum" (col-letter valcolix) reqt-row-nums) :subcategory)))
    (.getRowNum subcatrow)))

(defn- category
  "Adds a row and cells for the specified category - returns the index for this row"
  [cat sheet catrowix wave subcat-row-nums]
  (let [catrow (.createRow sheet catrowix)]
    (add-value-cell catrow 0 3 cat :category)
    (add-value-cells catrow 4 ["" ""] :category)
    (add-formula-cell catrow 6 (list-formula "sum" (col-letter 5) subcat-row-nums) :category)
    (doseq [pid (itemixseq (prod-keys wave))]
      (let [valcolix (+ 9 (* 3 (first pid)))]
        (add-value-cells catrow (- valcolix 2) ["" ""] :category)
        (add-formula-cell catrow valcolix (list-formula "sum" (col-letter valcolix) subcat-row-nums) :category)))
    (.getRowNum catrow)))

(defn- totals
  "Adds a row and cells for the grand total line"
  [sheet wave cat-row-nums]
  (let [totalrow (.createRow sheet (nextrowid))]
    (add-value-cell totalrow 3 6 "Totals" :totals)
    (doseq [pid (itemixseq (prod-keys wave))]
      (let [valcolix (+ 9 (* 3 (first pid)))]
        (add-formula-cell totalrow valcolix (list-formula "sum" (col-letter valcolix) cat-row-nums) :totals)))))



; TODO: break up this nested mess of maps and reduces into set of legible functions
(defn gen-excel
  "Returns a Workbook that represents the wave format in excel form"
  [wave]
  ; TODO: support both style workbooks HSSF and XSSF
  (let [wb (HSSFWorkbook.)
        sheet (.createSheet wb "Wave Analysis")]
      (init-styles wb)
      (doto sheet
        (.setDisplayGridlines true)
        (.setPrintGridlines true)
        (.setFitToPage true)
        (.setAutobreaks true))
      (doto (.getPrintSetup sheet)
        (.setLandscape true)
        (.setFitHeight 1)
        (.setFitWidth 1))
      (header sheet wave)
      (totals sheet wave 
        (reduce #(conj %1 %2) [] ; build vector of row numbers for categories
          (map ; convert from category name to line number (with cell/row addition side effects)
            (fn [c]
              (category c sheet (nextrowid) wave
                (reduce #(conj %1 %2) [] ; build a vector of row numbers of subcategories
                        (map ; convert from subcategory name to line number (with cell/row addition side effects)
                          (fn [subcat] 
                            (subcategory subcat sheet (nextrowid) wave
                            (reduce #(conj %1 %2) [];  build a vector of row numbers to be used in cell formulas
                                    (map  ;convert from requirements data structure to line numbers (with cell addition side effects)
                                      #(requirement sheet wave c subcat %) 
                                      (requirements wave c subcat)))))
                          (subcategories wave c)))))
            (categories wave))))
    (.autoSizeColumn sheet 2)
    (.autoSizeColumn sheet 3)
    wb))


