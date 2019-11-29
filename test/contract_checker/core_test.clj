(ns contract-checker.core-test
  (:require [clojure.test :refer :all]
            [contract-checker.core :as ccc]
            [clojure.data.json :as json]))


(defn- fail-rule
  "Always fails."
  [consumer-node producer-node]
  {:rule "fail-rule"
   :severity "minor"
   :description "I failed!"})


(def js1-producer (json/read-str (slurp "schema1.json") :key-fn keyword))
(def js2-producer (json/read-str (slurp "schema2.json") :key-fn keyword))


(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
