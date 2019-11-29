(ns contract-checker.core
  (:require [clojure.data.json :as json]))


(defn- node
  "gets the map of keys & values that represent the node"
  [js]
 (dissoc js :properties :items))


(defn- children
  "gets the children from the node."
  [js]
  (into {} (remove nil? (concat (:items js) (:properties js)))))


(defn- children-type
  "provides the key of the children."
  [js]
  (cond
    (not (nil? (:properties js))) :properties
    (not (nil? (:items js)))      :items
    :else                         nil))


(defn- has-children?
  "does the node have children?"
  [js]
  (not (nil? (children js))))


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

(defn- get-node
  "takes a nested clojure map, representing json and a path and returns the node at the path."
  [js path]
  (if (empty? path)
    js
    (get-node ((first path) js) (rest path))))


;; this is an example rule that we'll use as a default later
(defn- default-rule
  "Checks that the nodes are the same."
  [consumer-node producer-node]
  (if (= consumer-node producer-node)
    nil
    {:rule "default-rule"
     :severity "major"
     :description (str "consumer node: " consumer-node " and producer node: " producer-node
                       " are not the same!")}))


(defn- apply-rules
  [consumer-node producer-js path error rules]
  (let [producer-node (get-node producer-js path)]
    (if (nil? producer-node)
      ;; no corresponding producer-node. add an error
      (conj error {:path path
                   :rule "no corresponding producer node"
                   :description (str "no corresponding producer node found!")})
      ;; else e have a producer-node. apply all rules an collect any errors
      (reduce
       (fn [err current-rule]
         (let [result (current-rule consumer-node producer-node)]
           (if result
             (conj err (assoc result :path path))
             err)))
       error
       rules))))


(defn check-contract-impl
  ""
  [consumer-js producer-js path error rules]
  (let [chdn (children consumer-js)
        chdn-type (children-type consumer-js)]
    (if (empty? chdn)
      (let [new-error (apply-rules consumer-js producer-js path error rules)]
        new-error)
      (let [new-error (apply-rules consumer-js producer-js path error rules)]
        (conj error
              (mapcat
               (fn [[k v]]
                 (check-contract-impl
                  v
                  producer-js
                  (conj path chdn-type k)
                  new-error
                  rules))
               chdn))))))




(defn check-contract
  ""
  [consumer-js producer-js & {:keys  [rules] :or {rules [default-rule]}}]
  (check-contract-impl consumer-js producer-js [] [] rules))
