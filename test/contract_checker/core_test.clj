(ns contract-checker.core-test
  (:require [clojure.test :refer :all]
            [contract-checker.core :as cc]
            [contract-checker.rules :as rules]
            [clojure.data.json :as json]))


;; https://stackoverflow.com/questions/14488150/how-to-write-a-dissoc-in-command-for-clojure
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure
."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


(def default-rule cc/default-rule)
(def echo-rule cc/echo-rule)
(def check cc/check-contract)
(def attribute-optional rules/attribute-optional)
(def enum-values
 rules/enumeration-values)
(def string-length rules/string-length)
(def string-pattern rules/string-pattern)
(def numeric-range rules/numeric-range)
(def numeric-precision rules/numeric-precision)
(def min-cardinality rules/minimum-cardinality)
(def max-cardinality rules/maximum-cardinality)
(def type-checking rules/type-checking)


(defonce js1-producer (json/read-str (slurp "resources/schema1.json") :key-fn keyword))
(defonce js2-producer (json/read-str (slurp "resources/schema2.json") :key-fn keyword))
(defonce js1-consumer (dissoc-in js1-producer [:properties :lastName :type]))
(defonce js3-consumer (json/read-str (slurp "resources/schema3.json") :key-fn keyword))
(defonce js3-producer (json/read-str (slurp "resources/schema4.json") :key-fn keyword))


(deftest test1
  (testing "Producer and Consumer contracts the same"
    (is (= (check js1-producer js1-producer) '()))))


(deftest test2
  (testing "Producer and Consumer contracts the same"
    (let [js1-consumer (dissoc-in js1-producer [:properties :lastName :type])]
      (is (= (check js1-consumer js1-producer) 
             (list {:rule "default-rule",
                    :severity "major",
                    :description
                    (str "consumer node: {:description \"The person's last name.\"} and producer node: "
                         "{:type \"string\", :description \"The person's last name.\"} are not the same!"),
                    :path [:properties :lastName],
                    :consumer-node {:description "The person's last name."},
                    :producer-node
                    {:type "string", :description "The person's last name."}}))))))


(deftest test-attr-optional
  (let [consumer-node (get-in js3-consumer [:properties :role2 :items :properties :b1]) 
        producer-node consumer-node]
    (is (= (attribute-optional consumer-node consumer-node) 
           {:rule "attribute-name is optional"
            :severity "minor"
            :description (str "consumer node: " consumer-node " is optional!")}))))


(deftest test-enum-values
  (let [consumer-node (get-in js3-consumer [:properties :gender :items])
        producer-node (get-in js3-producer [:properties :gender :items])]
      (is (= (enum-values consumer-node producer-node)
             {:rule "enum values not same" 
              :severity "minor"
              :description (str "consumer node: " consumer-node
                                " has less enum values than producer node: " producer-node)}))))


(deftest test-string-length
  (let [consumer-node (get-in js3-consumer [:properties :firstName])
        producer-node (get-in js3-producer [:properties :firstName])]
    (is (= (string-length consumer-node producer-node) 
           {:rule "strength length changed" 
            :severity "minor"
            :description (str "consumer node: " consumer-node
                              " and producer node: " producer-node
                              " maximum string lengths are not the same!")}))))


(deftest test-numeric-range
  (let [consumer-node (get-in js3-consumer [:properties :age])
        producer-node (get-in js3-producer [:properties :age])]
    (is (= (numeric-range consumer-node producer-node) 
           {:rule "numeric range changed"
            :severity "major"
            :desciption (str "consumer node: " consumer-node
                             "and producer node: " producer-node
                             "numeric range aren't the same")}))))


(deftest test-numeric-precision
  (let [consumer-node (get-in js3-consumer [:properties :salary])
        producer-node (get-in js3-producer [:properties :salary])]
    (is (= (numeric-precision consumer-node producer-node)
           {:rule "numeric precision changed"
            :severity "major"
            :desciption (str "consumer node: " consumer-node
                             "and producer node: " producer-node
                             "numeric precision  aren't the same")}))))


(deftest test-min-cardinality
  (let [consumer-node (get-in js3-consumer [:properties :role2])
        producer-node (get-in js3-producer [:properties :role2])]
    (is (= (min-cardinality consumer-node producer-node) 
           {:rule "multiplicity changed" 
            :severity "minor"
            :description (str "consumer node: " consumer-node
                              " and producer node: " producer-node
                              " cardinality aren't the same!")}))))


(deftest test-max-cardinality
  (let [consumer-node (get-in js3-consumer [:properties :role2])
        producer-node (get-in js3-producer [:properties :role2])]
    (is (= (max-cardinality consumer-node producer-node) 
           {:rule "multiplicity changed" 
            :severity "major"
            :description (str "consumer node: " consumer-node
                              " and producer node: " producer-node
                              " cardinality aren't the same!")}))))


(deftest test-type-checking
  (let [consumer-node (get-in js3-consumer [:properties :salary])
        producer-node (get-in js3-producer [:properties :salary])]
    (is (= (type-checking consumer-node producer-node) nil))))


;; Demo stuff
(comment)

(def the-rules [attribute-optional enum-values string-length numeric-range numeric-precision min-cardinality type-checking ])

(check js3-consumer js3-producer :rules the-rules)
