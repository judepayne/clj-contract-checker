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


(defn- fail-rule
  "Always fails."
  [consumer-node producer-node]
  {:rule "fail-rule"
   :severity "minor"
   :description "I failed!"})


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


(defn- check-contract-impl
  "The implementation of check-contract."
  [consumer-js producer-js path error rules]
  (let [chdn (children consumer-js)
        chdn-type (children-type consumer-js)]
    (if (empty? chdn)
      (apply-rules consumer-js producer-js path error rules)
      (let [new-error (apply-rules consumer-js producer-js path error rules)]
        (concat error
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
  "Checks all specified rules against each node in the consumer contract vs
   the corresponding node in the produce contract and returns a collection of
   errors or nil if there are none. If a corresponding node cannot be found in
   the producer contract, an error is added."
  [consumer-js producer-js & {:keys  [rules] :or {rules [default-rule]}}]
  (distinct (check-contract-impl consumer-js producer-js [] [] rules)))
