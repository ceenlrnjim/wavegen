(ns wavegen.excel
  (:import [org.apache.poi.ss.usermodel Workbook CellStyle])
  (:import [org.apache.poi.hssf.usermodel HSSFWorkbook])
  (:import [org.apache.poi.ss.util CellRangeAddress])
  (:use [wavegen.core])
  (:use [wavegen.aggr]))

(def ^:dynamic *row-counter* (atom -1))
(def cellstyles (atom {}))

; TODO: add style details
(defn- header-style
  [wb]
  (let [style (.createCellStyle wb)]
    (doto style
      (.setAlignment CellStyle/ALIGN_CENTER))))

(defn- init-styles
  [wb]
  (swap! cellstyles assoc :header (header-style wb)))

(defn- addcell
  "Adds a cell with the specified value and style key to the specified sheet.
  Style key is used as the key to the cellstyles map.  returns the cell"
  ([row cellix value stylekey]
    (let [cell (.createCell row cellix)]
      (.setCellValue cell value)
      (.setCellStyle cell (get @cellstyles stylekey))
      cell))
  ; Adds a row that spans the specified cell indices
  ([row cellstartix cellendix value stylekey]
    (let [cell (addcell row cellstartix value stylekey)]
      (.addMergedRegion (.getSheet row) (CellRangeAddress. (.getRowNum row) (.getRowNum row) cellstartix cellendix))
      cell)))

(defn- nextrowid
  "Returns next row id as an int"
  []
  (int (swap! *row-counter* inc)))

(defn- header
  "writes the header rows to the specified sheet for the specified wave"
  [sheet wave]
  (let [header1rowix (nextrowid)
        header2rowix (nextrowid)
        header1row (.createRow sheet header1rowix)
        header2row (.createRow sheet header2rowix)]
    (addcell header1row 0 3 "" :header)
    (addcell header1row 4 6 "Weightings" :header)
    (addcell header2row 0 "Category" :header)
    (addcell header2row 1 "Sub-category" :header)
    (addcell header2row 2 "Requirement" :header)
    (addcell header2row 3 "Evaluation Criteria" :header)
    (addcell header2row 4 "Reqt" :header)
    (addcell header2row 5 "Sub-cat" :header)
    (addcell header2row 6 "Category" :header)
    (let [cell-counter (atom 4)]
      (doseq [pid (prod-keys wave)]
        (let [startix (swap! cell-counter + 3)
              endix (+ 2 startix)]
          (addcell header1row startix endix (product-desc wave pid) :header)
          (addcell header2row startix "Score" :header)
          (addcell header2row (inc startix) "Notes" :header)
          (addcell header2row (+ 2 startix) "Wgt Score" :header))))))


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
    wb))


