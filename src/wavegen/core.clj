(ns wavegen.core
  (:require [clojure.tools.logging :as log]))

(defn empty-wave
[name]
{:name name :products {} :requirements {} :scores {}}) ; what is the ideal structure for scores

(defmacro with-wave 
  [name & body]
  `(-> (empty-wave ~name) ~@body))

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

(defn- bucket-reqts
  [wave]
  (group-by :categories (vals (:requirements wave))))

(defn- normalize-weights
  "Takes a sequence requirements and returns a map of id to percentage weight (relative to the whole wave)"
  [reqts]
  (let [total (reduce #(+ %1 (:wt %2)) 0 reqts)]
    (loop [m {}
           s reqts]
      (if (empty? s) m (recur (assoc m (:id (first s)) (/ (:wt (first s)) total)) (rest s))))))

(defn- compute-product-scores
  [wave reqt-id weight]
  (map
    (fn [x]
      (let [raw (get-score wave (:id x)  reqt-id)]
        (vector (* weight raw) raw)))
    (vals (:products wave))))

(defn- compute-scores
  "Takes a bucketted map of requirements and adds the product-scores key with a vector of weighted and unweighted scores"
  [wave]
  (let [reqts (vals (:requirements wave))
        weights (normalize-weights reqts)]
    (map 
      #(assoc % :computed-scores [])
      reqts)))



