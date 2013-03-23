(ns wavegen.html
  (:use [wavegen.core])
  (:use [wavegen.aggr]))

(defn branch?
  [n]
  (contains? #{:wave :category :subcategory} (:type n)))

(defmulti node-children :type)
(defmethod node-children :wave [{:keys [wave]}]
  (map #(assoc {} :wave wave 
                  :category % 
                  :type :category) 
        (categories wave)))

(defmethod node-children :category [{:keys [wave category]}]
  (map #(assoc {} :wave wave 
                  :category category 
                  :type :subcategory 
                  :subcategory %) 
       (subcategories wave category)))

(defmethod node-children :subcategory [{:keys [wave category subcategory]}]
  (map #(assoc {} :wave wave
                  :category category
                  :subcategory subcategory
                  :requirement %
                  :type :requirement)
       (requirements wave category subcategory)))

; returns a sequence of streams for the wave hierarchy
; TODO: how to address before/after open/close tags and lines
(defmulti render-before :type)
(defmethod render-before :wave [node]
  ["before wave"])
(defmethod render-before :category [node]
  ["before category"])
(defmethod render-before :subcategory [node]
  ["before subcat"])
(defmethod render-before :requirement [node]
  ["before reqt"])
(defmethod render-before :totals [node]
  ["before totals"])

(defmulti render-after :type)
(defmethod render-after :wave [node]
  ["after wave"])
(defmethod render-after :category [node]
  ["after category"])
(defmethod render-after :subcategory [node]
  ["after subcat"])
(defmethod render-after :requirement [node]
  ["after reqt"])
(defmethod render-after :totals [node]
  ["after totals"])

(defn render-tree
  "Returns a sequence of strings for the tree"
  [node]
  (let [b (render-before node)
        a (render-after node)]
    (if 
      (branch? node) 
      (flatten [b (map render-tree (node-children node)) a])
      [b a])))

(defn gen-html
  "returns a string containing the html representation of the specified wave"
  [wave]
  (let [s (render-tree {:wave wave :type :wave})]
    (println s)
    (apply str s)))
