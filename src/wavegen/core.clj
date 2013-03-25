(ns wavegen.core
  (:require [rels]))

(def ootb {2 "Out of the box feature" 1 "Possible with limited effort" 0 "Not possible or requires significant effort"})
(def yesno {2 "Yes" 0 "No"})
;
; TODO: could we have an alternate implementation of these functions that does the calculations as we go along?
(defn empty-wave
  [name]
  {:name name :products [] :requirements [] :scores []}) ; what is the ideal structure for scores

(defmacro with-wave 
  [name & body]
  `(-> (empty-wave ~name) ~@body))

; TODO: add standard score map definitions (e.g. ootb)

(defn reqt 
  "Adds a requirement to the wave currently in context.  Must be executed in a with-wave block"
  [wave id description abs-weight score-map cat subcat]
  (assoc wave :requirements (conj (:requirements wave) {:reqtid id :reqtdesc description :wt abs-weight :score-key score-map :category cat :subcategory subcat})))

(defn product 
  "Adds a product to the wave"
  [wave id description]
  (assoc wave :products (conj (:products wave) {:prodid id :proddesc description})))

(defn score
   "Adds a score for a product"
   [wave prod-id & reqt-score-pairs]
   (if
     (empty? reqt-score-pairs) wave
     (recur (assoc wave :scores 
                   (conj (:scores wave) {:prodid prod-id :reqtid (first reqt-score-pairs) :score (second reqt-score-pairs)}))
            prod-id
            (drop 2 reqt-score-pairs))))

(defn get-score [wave prod-id reqt-id]
  (rels/select-single (:scores wave) 0 :prodid prod-id))

(defn product-desc [wave prod-id]
  (:desc (rels/select-single (:products wave) {} :prodid prod-id)))

