(ns wavegen.excel
  (:require [rels]))

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


(def specs
  {:requirement 
    { :reqt [:reqtdesc]
      :crit [:score-key]
      :wtd  [(relwt-formula :raw)]
      :products 
        { :score-wtd [(infix-binary-formula "*" :score :wtd)] }}
   :category
    {:cat [:category {:mergecnt 4}]
     :cat-wtd [(sum-subcat-formula :sub-wtd)] ;
     :products 
      {:score-wtd [(sum-subcat-formula :score-wtd)] }}
   :subcategory
    {:subcat [:subcategory {:mergecnt 3}]
     :sub-wtd [(sum-reqt-formula :wtd)]
     :products
      { :score-wtd [(sum-reqt-formula :score-wtd)] }}
      ; TODO: headers, subheaders, totals
   :header
     { :raw ["Weightings" {:mergecnt 4}]
       :products { :score [:proddesc {:mergecnt 3}] }}
   :subheader
     { :cat ["Category"]
       :subcat ["Sub-Category"]
       :reqt ["Requirement"]
       :crit ["Evaluation Criteria"]
       :raw ["Abs Wt"]
       :wtd ["Rel Wt"]
       :sub-wtd ["Sub-cat"]
       :cat-wtd ["Categoty"]
       :products { :score ["Score"] 
                   :notes ["Notes"]
                   :score-wtd ["Wtd Score"] }}
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

(defn build-wave-relation
  "Combines the data in the specified wave, computes score values, and returns a consolidated relation
   for rendering the wave report"
  [{:keys [products requirements]}]
     (-> (rels/append requirements (fn [t] {:type :requirement :score-key (str (:score-key t))}))
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
  [row cix {:keys [mergecnt]}]
  (let [c (.createCell row cix)]
    ; TODO: other style support
    (when mergecnt (merge-cols row cix (+ cix (- mergecnt 1))))
    c))

(defn set-cell-value
  [c v]
  (.setCellValue c v))

(defn set-cell-formula
  [c formula]
  (.setCellFormula c formula))

(defn set-row-contents
  [row spec col-ixs wave data]
  (doseq [[k [v styles]] spec]
    (let [cell (make-cell row (get col-ixs k) styles)]
      (cond (fn? v) (set-cell-formula cell (v wave data col-ixs))
            (keyword? v) (set-cell-value cell (get data v))
            :else (set-cell-value cell v)))))

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


(defn gen-xlsx
  [wave]
  (let [wb (org.apache.poi.xssf.usermodel.XSSFWorkbook.)
        sheet (.createSheet wb)
        waverel (build-wave-relation wave)
        cols [:rownum :cat :subcat :reqt :crit :raw :wtd :sub-wtd :cat-wtd]
        prod-cols [:score :notes :score-wtd]
        rowcounter (atom 0)]
    (doseq [data waverel]
      (let [row (.createRow sheet @rowcounter)]
          (build-row cols prod-cols data {:products (:products wave) :book wb :sheet sheet :row row :wave waverel}))
      (swap! rowcounter + 1))
    wb))
