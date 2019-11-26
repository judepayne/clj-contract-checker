(ns contract-checker.core
  (:require [clojure.data.json :as json]))


(defn node
  "gets the map of keys & values that represent the node"
  [js]
 (dissoc js :properties))


(defn children
  "gets the children from the node."
  [js]
  (:properties js))


(defn has-children?
  "does the node have children?"
  [js]
  (not (nil? (:properties js))))


;; design thoughts
;; check-contract function - takes a producer schema, a consumer schema and ruleset
;; return: nil if no errors or a collection of errors found
;; how it should work:
;; start with the consumer contract (as a tree - nested clojure maps) and an empty error
;; collection. We recurse down through the consumer tree, for each consumer node:
;; -- find a match/ identify if we have the node in the producer tree. (check if we have keyword)
;;  -- if not, add an error and return
;;  -- if yes, check the ruleset - accumulate any errors
;;         check if has children. return if not or
;;         recurse into children

;; path is defined as a vector of keywords that are applied in sequence to navigate down from the root node. e.g. [:properties :firstName]

(defn get-node
  "takes a nested clojure map, representing json and a path and returns the node at the path."
  [js path]
  (if (empty? path)
    js
    (get-node ((first path) js) (rest path))))

;; a ruleset is a collection of rules to be applied to a pair of nodes

(defn same-type-rule
  "Do both nodes have a type and the types are the same."
  [consumer-node producer-node]
  (let [con-type (:type consumer-node)
        prod-type (:type producer-node)]
    (if
        (and (not (nil? con-type))
             (= con-type prod-type))
      nil
      {:rule "same-type-rule"
       :description (str
                     "conumer type: " con-type " is not equal to producer type " prod-type
                     ". Types should both not be nil and must be the same.")})))


(def apply-rules
  [consumer-node producer-node path error]
  ;; TODO
)
