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
;(def echo-rule cc/echo-rule)
(def check cc/check-contract)
(def class-rename rules/class-renamed-rule)
(def attribute-optional rules/attribute-optional-rule)
(def enum-values rules/enumeration-values-rule)
(def string-length rules/string-length-rule)
(def string-pattern rules/string-pattern-rule)
(def numeric-range rules/numeric-range-rule)
(def numeric-precision rules/numeric-precision-rule)
(def min-cardinality rules/minimum-cardinality-rule)
(def max-cardinality rules/maximum-cardinality-rule)
(def type-checking rules/type-checking-rule)
(def keys-same rules/keys-same-rule)

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

 
(deftest test-class-rename
  (let [consumer-node (get-in js3-consumer [:$id])
        producer-node (get-in js3-producer [:$id])]
    (is (= (class-rename consumer-node producer-node [])) nil)))


(deftest test-attr-optional
  (let [consumer-node (get-in js3-consumer [:properties :role2 :items :properties :b1])
        producer-node consumer-node]
    (is (= (attribute-optional consumer-node producer-node [:properties :role2 :items :properties :b1]) 
           {:rule "Attribute name is optional"
            :severity "Minor"
            :description (str "Consumer node: " consumer-node " is optional!")
            :path [:properties :role2 :items :properties :b1 :type]}))))


(deftest test-enum-values
  (let [consumer-node (get-in js3-consumer [:properties :gender :items])
        producer-node (get-in js3-producer [:properties :gender :items])]
      (is (= (enum-values consumer-node producer-node [:properties :gender :items])
             {:rule "Enum values are not same" 
              :severity "Major"
              :description (str "Consumer node: " consumer-node
                                " has less enum values than producer node: " producer-node)
              :path [:properties :gender :items :enum]}))))


(deftest test-string-length
  (let [consumer-node (get-in js3-consumer [:properties :firstName])
        producer-node (get-in js3-producer [:properties :firstName])]
    (is (= (string-length consumer-node producer-node [:properties :firstName]) 
           {:rule "Strength length changed" 
            :severity "Minor"
            :description (str "Consumer node: " consumer-node
                              " and producer node: " producer-node
                              " maximum string lengths are not the same!")
            :path [:properties :firstName :maxLength]}))))


(deftest test-numeric-range
  (let [consumer-node (get-in js3-consumer [:properties :age])
        producer-node (get-in js3-producer [:properties :age])]
    (is (= (numeric-range consumer-node producer-node [:properties :age]) 
           {:rule "Numeric range changed"
            :severity "Major"
            :desciption (str "Consumer node: " consumer-node
                             "and producer node: " producer-node
                             "numeric range aren't the same")
            :path [:properties :age :maximum]}))))


(deftest test-numeric-precision
  (let [consumer-node (get-in js3-consumer [:properties :salary])
        producer-node (get-in js3-producer [:properties :salary])]
    (is (= (numeric-precision consumer-node producer-node [:properties :salary])
           {:rule "Numeric precision changed"
            :severity "Major"
            :desciption (str "Consumer node: " consumer-node
                             "and producer node: " producer-node
                             "numeric precision  aren't the same")
            :path [:properties :salary :multipleOf]}))))


(deftest test-min-cardinality
  (let [consumer-node (get-in js3-consumer [:properties :role2])
        producer-node (get-in js3-producer [:properties :role2])]
    (is (= (min-cardinality consumer-node producer-node [:properties :role2]) 
           {:rule "Multiplicity changed" 
            :severity "Minor"
            :description (str "Consumer node: " consumer-node
                              " and producer node: " producer-node
                              " cardinality aren't the same!")
            :path [:properties :role2 :minItems]}))))


(deftest test-max-cardinality
  (let [consumer-node (get-in js3-consumer [:properties :role2])
        producer-node (get-in js3-producer [:properties :role2])]
    (is (= (max-cardinality consumer-node producer-node [:properties :role2]) 
           {:rule "Multiplicity changed" 
            :severity "Major"
            :description (str "Consumer node: " consumer-node
                              " and producer node: " producer-node
                              " cardinality aren't the same!")
            :path [:properties :role2 :maxItems]}))))


(deftest test-type-checking
  (let [consumer-node (get-in js3-consumer [:properties :salary])
        producer-node (get-in js3-producer [:properties :salary])]
    (is (= (type-checking consumer-node producer-node [:properties :salary]) nil))))


(deftest test-keys-same
  (let [consumer-node (get-in js3-consumer [:properties :gender])
        producer-node (get-in js3-producer [:properties :gender])]
    (is (= (keys-same consumer-node producer-node [:properties :gender])
           nil))))



;; Demo stuff
(comment

  (def the-rules [class-rename attribute-optional enum-values string-length numeric-range numeric-precision min-cardinality max-cardinality type-checking keys-same ])

  (check js3-consumer js3-producer :rules the-rules))
