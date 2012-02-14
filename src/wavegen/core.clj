(ns wavegen.core
  (:require [clojure.tools.logging :as log]))

(def ootb {2 "Out of the box feature" 1 "Possible with limited effort" 0 "Not possible or requires significant effort"})
(def yesno {2 "Yes" 0 "No"})
;
; TODO: could we have an alternate implementation of these functions that does the calculations as we go along?
(defn empty-wave
[name]
{:name name :products {} :requirements {} :scores {}}) ; what is the ideal structure for scores

(defmacro with-wave 
  [name & body]
  `(-> (empty-wave ~name) ~@body))

; TODO: add standard score map definitions (e.g. ootb)

(defn reqt 
  "Adds a requirement to the wave currently in context.  Must be executed in a with-wave block"
  [wave id description abs-weight score-map & categories]
  (assoc wave :requirements (assoc (:requirements wave) id {:id id :desc description :wt abs-weight :scores score-map :categories categories})))

(defn product 
  "Adds a product to the wave"
  [wave id description]
  (assoc wave :products (assoc (:products wave) id {:id id :desc description})))

(defn score
   "Adds a score for a product"
   [wave prod-id & reqt-score-pairs]
   (if
     (empty? reqt-score-pairs) wave
     (recur (assoc wave :scores (assoc (:scores wave) [prod-id (first reqt-score-pairs)] (second reqt-score-pairs)))
            prod-id
            (rest (rest reqt-score-pairs)))))

(defn get-score [wave prod-id reqt-id]
  (get (get wave :scores) [prod-id reqt-id]))
