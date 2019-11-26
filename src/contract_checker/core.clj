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
  (println (first path) (rest path))
  (if (empty? path)
    js
    (get-node ((first path) js) (rest path))))

;; a ruleset is a collection of rules to be applied to a pair of nodes

(defn type-same
  "Do both nodes have a type and the types are the same."
  [node1 node2]
  (if
      (and (not (nil? (:type node1)))
           (= (:type node1) (:type node2)))
    nil
    {:path 
     :rule "type-same"
     :description "types should both not be nil and must be the same."}))
