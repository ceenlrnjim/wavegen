(ns wavegen.html
  (:use [wavegen.core]))

(def formatter (java.text.DecimalFormat. "#0.0#"))
(defn decfmt 
  "Returns the specific number as a standard format string"
  [d] (.format formatter d))

(defn template-map
  "template is a sequence of strings and keywords.  Keywords are replaced with associated values in map m"
  [template m]
  (apply str 
         (map 
           (fn [t]
             (cond (and (keyword? t) (number? (get m t))) (decfmt (get m t))
                   (keyword? t) (get m t)
                   :else t))
           template)))

(def templates
  {:requirement
    {:header ["<tr class='requirement'><td class='linenum'>" :linenum "</td><td/><td/><td>" :reqtdesc "</td><td class='criteria'>" :score-key "</td><td>" :rel-wt "</td><td/><td/>"]
     :product ["<td>" :score "</td><td/><td>" :rel-score "</td>"]
     :terminator ["</tr>\n"]}
   :subcategory
    {:header ["<tr class='subcategory'><td class='linenum'>" :linenum "</td><td/><td colspan='3' class='subcategory-name'>" :subcategory "</td><td/><td class='subcategory-weight'>" :rel-wt "</td><td/>"]
     :product ["<td/><td/><td>" :score "</td>"]
     :terminator ["</tr>\n"]}
   :category
    {:header ["<tr class='category'><td class='linenum'>" :linenum "</td><td colspan='4' class='category-name'>" :category "</td><td/><td/><td class='category-weight'>" :rel-wt "</td>"]
     :product ["<td/><td/><td>" :score "</td>"]
     :terminator ["</tr>\n"]}
   :totals
    {:header ["<tr class='total'><td/><td/><td/><td/><td colspan='4'>Final Score:</td>"]
     :product ["<td/><td/><td>" :score "</td>"]
     :terminator ["</tr>\n"]}
   :header
    {:header ["<tr class='header'><td colspan='5'></td><td colspan='3'>Weightings</td>"]
     :product ["<td colspan='3'>" :proddesc "</td>"]
     :terminator ["</tr>\n"]}
   :subheader
    {:header ["<tr class='header'><td class='linenumlabel'></td><td class='categorylabel'>Category</td><td class='subcategorylabel'>Sub-category</td><td class='reqtlabel'>Requirement</td><td class='criterialabel'>Evaluation Criteria</td><td class='reqtwtlabel'>Reqt</td><td class='subcatwtlabel'>Sub-cat</td><td class='catwtlabel'>Category</td>"]
     :product ["<td class='scorelabel'>Score</td><td class='noteslabel'>Notes</td><td class='wtscorelabel'>Wgt Score</td>"]
     :terminator ["</tr>\n"]}
   :pagehead "<html><head><link rel='stylesheet' href='wave.css'></head><body><table><tbody>"
   :pagefooter "</tbody></table></body></html>"})

(defn aggr-scores
  "computes a list of the aggregated scores for each product in the relation w where (pred %) is true"
  [pred w]
  (-> (rels/select w pred)
      ((comp flatten rels/col-seq) :scores)
      ; the contents of scores is a sequence of maps, which is itself a relation
      (rels/project [:prodid :rel-score])
      (#(group-by %2 %1) :prodid)
      ; aggregate the score for each product TODO: need to make sure products are in the same order as elsewhere
      (#(map %2 %1) (fn [[k v]] {:score (reduce #(+ (:rel-score %2) %1) 0 v)}))))

(defn catpred [c]
  "Returns a predicate function that will return true for category rows of the specified name"
  (fn [t] (= (:category t) c)))

(defn subcatpred [c s]
  "Returns a predicate function that will return true for subcategory rows of the specified name in the specified category"
  (fn [t] (and ((catpred c) t) (= (:subcategory t) s))))

(defn aggr-weight
  "Returns the aggregated weight of rows that match the specified predicate"
  [pred w]
  (reduce + 0 (rels/col-seq
                (rels/select w pred)
                :rel-wt)))

(defn conj-category-rows
  "Adds a row to the relation for each of the categories and subcategories associated with the requirements"
  [waverel]
  (let [cats (into #{} (rels/col-seq waverel :category))
        subcats (into #{} (rels/project waverel [:category :subcategory]))]
    (concat waverel
      (map (fn [c] {:type :category 
                    :category c 
                    :rel-wt (aggr-weight (catpred c) waverel) 
                    :scores (aggr-scores (catpred c) waverel)}) 
           cats)
      (map (fn [{c :category s :subcategory}] 
             {:type :subcategory 
              :category c 
              :subcategory s 
              :rel-wt (aggr-weight (subcatpred c s) waverel)
              :scores (aggr-scores (subcatpred c s) waverel)})
           subcats))))

(defn conj-totals
  "Adds a total row to the specified wave relation"
  [w]
  (concat w 
    [{:type :totals :scores (aggr-scores #(= (:type %) :requirement) w)}]))

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
  [{:keys [scores products requirements]}]
  (let [total-weight (reduce + 0 (rels/col-seq requirements :wt))]
     (-> (rels/append requirements (fn [r] {:rel-wt (/ (:wt r) total-weight) :type :requirement })) ; compute relative weight, mark row as a requirement
         (rels/join scores [= :reqtid :reqtid])                                                     ; join to scores
         (rels/append (fn [rs] {:rel-score (* (:rel-wt rs) (:score rs))}))                          ; compute relative score
         (rels/join products [= :prodid :prodid])                                                   ; join to products
         (rels/denormalize :scores :prodid :proddesc :score :rel-score)                             ; denormailize to one row per requirement
         (conj-category-rows)                                                                       ; add rows for categories and subcategories
         (#(sort-by %2 %1) #(vector (:category %) (:subcategory %) (:reqtdesc %)))                  ; sort the rows into the right order
         (conj-totals)                                                                              ; add row for totals at the end
         (cons-headers products))))                                                                 ; add rows at the beginning with the headers 

(defn render
  [row]
  (let [{:keys [header product terminator]} (get templates (:type row))
        h (template-map header row)
        ps (map #(template-map product %) (get row :scores))
        t (template-map terminator row)]
    (apply str (flatten [h ps t]))))

(defn gen-html
  "returns a string containing the HTML representation of the wave"
  [wave]
  (apply str 
    (flatten [(:pagehead templates)
              (map render (build-wave-relation wave))
              (:pagefooter templates)])))
