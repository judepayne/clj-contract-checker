(ns contract-checker.core-test
  (:require [clojure.test :refer :all]
            [contract-checker.core :as cc]
            [clojure.data.json :as json]))


;; https://stackoverflow.com/questions/14488150/how-to-write-a-dissoc-in-command-for-clojure
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


(defn fail-rule
  "Always fails."
  [consumer-node producer-node]
  {:rule "fail-rule"
   :severity "minor"
   :description "I failed!"})


(def default-rule cc/default-rule)
(def echo-rule cc/echo-rule)
(def check cc/check-contract)


(def js1-producer (json/read-str (slurp "resources/schema1.json") :key-fn keyword))
(def js2-producer (json/read-str (slurp "resources/schema2.json") :key-fn keyword))
(def js1-consumer (dissoc-in js1-producer [:properties :lastName :type]))


(deftest test1
  (testing "Producer and Consumer contracts the same"
    (is (= (check js1-producer js1-producer) '()))))


(deftest test2
  (testing "Producer and Consumer contracts the same"
    (let [js1-consumer (dissoc-in js1-producer [:properties :lastName :type])]
      (is (= (check js1-consumer js1-producer) 
             '({:rule "default-rule",
                :severity "major",
                :description
                "consumer node: {:description \"The person's last name.\"} and producer node: {:type \"string\", :description \"The person's last name.\"} are not the same!",
                :path [:properties :lastName],
                :consumer-node {:description "The person's last name."},
                :producer-node
                {:type "string", :description "The person's last name."}}))))))
