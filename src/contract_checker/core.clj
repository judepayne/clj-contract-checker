(ns contract-checker.core
  (:require [clojure.data.json :as json]))


;; ********************* Notes on the design ******************
;; Json-schema is a tree of nested maps. Note that only contained maps are used to indicate
;; lower/ children levels.

;; We define a *path* (down through the nested map) as a vector of keywords which are applied
;; in sequence to navigate down from the root node. e.g. [:properties :firstName]
;; ************************************************************

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
  [consumer-node-pruned producer-js keys-to-remove path error rules]
  (let [producer-node (get-node producer-js path)]
    (if (nil? producer-node)
      ;; no corresponding producer-node. add an error
      (conj error {:path path
                   :rule "no corresponding producer node"
                   :description (str "no corresponding producer node found!")})
      ;; else we have a producer-node. prune it of any mapentries that lead to
      ;; children levels, before applying all rules and collecting the errors
      (let [producer-node-pruned (apply dissoc producer-node keys-to-remove)]
        (reduce
         (fn [err current-rule]
           (let [result (current-rule consumer-node-pruned producer-node-pruned)]
             (if result
               (conj err (assoc result :path path))
               err)))
         error
         rules)))))


(defn- recurse-down
  "The implementation of check-contract"
  [m producer-js path error rules]
  (let [next-gen         (filter
                          (fn [[k v]] (map? v))  ;; the mapentries that lead to children levels
                          m)
        keys-to-remove    (map first next-gen)  ;; the keys of those mapentries
        cur-node          (apply dissoc m keys-to-remove)  ;; remove them from cur-node...
                          ;; ...before checking for errors if cur-node is not empty
        new-error         (if (empty? cur-node)
                            error
                            (apply-rules cur-node producer-js keys-to-remove path error rules))]
    (if (empty? next-gen)
      (conj error new-error)
      (concat error
              (mapcat
               (fn [[k v]]
                 (recurse-down v producer-js (conj path k) new-error rules))
               next-gen)))))


(defn check-contract
  "Checks all specified rules against each node in the consumer contract vs
   the corresponding node in the produce contract and returns a collection of
   errors or nil if there are none. If a corresponding node cannot be found in
   the producer contract, an error is added."
  [consumer-js producer-js & {:keys  [rules] :or {rules [default-rule]}}]
  (distinct (flatten (recurse-down consumer-js producer-js [] [] rules))))
