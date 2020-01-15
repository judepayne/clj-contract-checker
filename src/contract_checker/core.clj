(ns contract-checker.core
  (:require [clojure.data.json      :as json]
            [contract-checker.rules :as rules]
            [rhizome.viz :as v]))

;; ---------------------------------- Notes on the design -----------------------------------
;; Json-schema is a tree of mainly nested maps (vectors are also used sometimes).
;; The existence of nested/ maps vectors are used to denote further children nodes.
;; At any given node, we need to separate the map entries which represent that node itself
;; from *structural* entries, i.e. those used to denote further children nodes.

;; We define a *path* (down through the nested map) as a vector of keywords which are applied
;; in sequence to navigate down from the root node. e.g. [:properties :firstName]
;; 
;; ------------------------------------------------------------------------------------------

(defn- json-schema-non-structural-vector?
  "Is the supplied key one of those used in json-schema to indicate a
   non-structural (i.e. doesn't contain child nodes) vector."
  [k]
  (some? (some #{k} [:enum :required :type])))


(defn- node?
  "Is this map-entry part of the node - i.e. not a *structural* entry
   that leads to other child nodes."
  [[k v]]
  (and (not (map? v))
       (or (not (vector? v))
           (json-schema-non-structural-vector? k))))


(defn node
  "Returns the node part of the map. Returns a (sub)map"
  [m]
  (if (map? m) ;; guard in case map not passed
    (into {} (get (group-by node? m) true))
    m))


(defn structural
  "Returns the structural map entries in the map. Returns a sequence of 1 entry maps."
  [m]
  (if (map? m) ;; guard in case map not passed
    (into {} (get (group-by node? m) false))
    m))


(defn- get-node
  "takes a nested clojure map, representing json and a path and returns the node at the path."
  [js path]
  (if (empty? path)
    js
    (if (sequential? js)  ;; If node is sequential, merge the maps within and recurse.
      (let [merged-contents (apply merge js)]
        (get-node merged-contents path))
      (get-node ((first path) js) (rest path)))))


;; this is an example rule that we'll use as a default later
(defn default-rule
  "Checks that the nodes are the same."
  [consumer-node producer-node]
  (if (= consumer-node producer-node)
    nil
    {:rule "default-rule"
     :severity "major"
     :description (str "consumer node: " consumer-node " and producer node: " producer-node
                       " are not the same!")}))

;; A simple rule to be used in the dev process
(defn echo-rule
  "Always fails."
  [consumer-node producer-node]
  {:rule "echo-rule"})


(defn apply-rule
  "Applies a single rule to a consumer node given the overall producer-node."
  [rule consumer-node producer-node]
  (if (rules/contains? (:type consumer-node) "null")
    nil
    (rule consumer-node producer-node)))


(defn- apply-rules
  "Uses path to navigate to producer-node. Removes structural keys (that lead to
  children nodes) from the producer node and applies all comparison rules to the
  consumer node (which has been passed with structural keys removed) and producer node,
  collecting any comparisons that fail in an error vector and returning it. "
  [consumer-node-pruned producer-js keys-to-remove path error rules]
  (let [producer-node (get-node producer-js path)
        producer-node-fixed  ;; If node is sequential, merge the maps within.
                             (if (sequential? producer-node)
                               (apply merge producer-node)
                               producer-node)]
    (if (nil? producer-node-fixed)
      ;; no corresponding producer-node. add an error
      (conj error {:path path
                   :rule "no corresponding producer node"
                   :description (str "no corresponding producer node found!")})
      ;; else we have a producer-node. prune it of any mapentries that lead to
      ;; children levels, before applying all rules and collecting the errors
      (let [producer-node-pruned (apply dissoc producer-node-fixed keys-to-remove)]
        (reduce
         (fn [err current-rule]
           (let [result (apply-rule current-rule consumer-node-pruned producer-node-pruned)]
             (if result
               (conj err (assoc result :path path
                                       :consumer-node consumer-node-pruned
                                       :producer-node producer-node-pruned))
               err)))
         error
         rules)))))


(defn- down
  "The implementation of check-contract."
  [item prod-js path error rules]
  (cond
    (map? item)         ;; partition node and not-nodes. apply-rules to node, get new-error
                        ;; and map/cat recurse for non-nodes
                        (let [nd (node item)
                              struc-items (structural item)
                              struc-items-ks (map first struc-items)
                              new-error (if (empty? nd)
                                          error
                                          (apply-rules nd prod-js struc-items-ks
                                            path error rules))]
                          (if (empty? struc-items)
                            new-error
                            (map
                              (fn [n] (down n prod-js path new-error rules))
                               struc-items)))


    (map-entry? item)   ;; add k to path and recurse
                        (down (val item) prod-js (conj path (key item)) error rules)


    (sequential? item)
                        (down (apply merge item) prod-js path error rules)


    :else               ;; return existing error. nothing to do here.
                        error))


(defn check-contract
  "Checks all specified rules against each node in the consumer contract vs
   the corresponding node in the produce contract and returns a collection of
   errors or nil if there are none. If a corresponding node cannot be found in
   the producer contract, an error is added."
  [consumer-js producer-js & {:keys  [rules] :or {rules [default-rule]}}]
  (distinct (flatten (down consumer-js producer-js [] [] rules))))


;; For visualization of a json-schema

(defn- split-map
  "Splits a map of n entries into a sequence of n 1-entry maps."
  [m]
  (reduce
   (fn [acc [k v]]
     (conj acc {k v}))
   []
   m))


(defn- children
  "Returns the children of the node."
  [n]
  (cond
    (sequential? n) n

    (map? n) (if (empty? (node n))
               ;; i.e. structural entries only
               (let [f (val (first n))]    ;; - TODO change to handle n > 1
                 (if (sequential? f) f
                     (split-map f)))
               ;; split out the structural elements
               (split-map (structural n)))))


(defn- map->string
  "Creates a formatted string representation of the map."
  [m]
  (if (string? m) m ;; guard in case map not passed.
      (reduce
       (fn [acc [k v]]
         (str acc k " " v "\n"))
       ""
       m)))


(defn- seq->string
  "Creates a formatted string representation of the seq"
  [s]
  (reduce
   (fn [acc cur]
     (str acc cur))
   ""
   s))


(def graphviz-node-options
  {:style "filled, rounded"
   :fontsize 10
   :shape "rect"})


(defn viz
  "Displays a visualization of the json-schema, if Graphviz is installed."
  [js]
  (if (and (empty? (node js)) (> (count js) 1))
    (viz {"{ }" js}) ;; catch when first node only has structural elements.
    (v/view-tree
     (fn [n] (not (empty? (structural n))))
     children
     js
     :node->descriptor (fn [n] (if (empty? (node n))
                                 (merge graphviz-node-options {:label (seq->string (keys n))
                                                               :fillcolor "snow"})
                                 (merge graphviz-node-options {:label (map->string (node n))
                                                               :fillcolor "lightsteelblue1"}))))))


(defn viz-svg
  "Returns svg visualization of the json-schema, if Graphviz is installed."
  [js]
  (if (and (empty? (node js)) (> (count js) 1))
    (viz {"{ }" js}) ;; catch when first node only has structural elements.
    (v/tree->svg
     (fn [n] (not (empty? (structural n))))
     children
     js
     :node->descriptor (fn [n] (if (empty? (node n))
                                 (merge graphviz-node-options {:label (seq->string (keys n))
                                                               :fillcolor "snow"})
                                 (merge graphviz-node-options {:label (map->string (node n))
                                                               :fillcolor "lightsteelblue1"}))))))
