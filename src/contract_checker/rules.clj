(ns contract-checker.rules
  (:require [clojure.data.json      :as json])
  (:refer-clojure :exclude [contains?]))


(defn contains?
  [coll item]
 (cond
   (map? coll)  (clojure.core/contains? coll item)
   (sequential? coll) (some? (some #{item} coll))
   :default      false))


(defn attr-present
  [consumer-contract producer-contract attr-name]
  (if (or (and (contains? consumer-contract attr-name) (not (contains? producer-contract attr-name)))
          (and (contains? producer-contract attr-name) (not (contains? consumer-contract attr-name))))
    (do 
      (println (str "Warning!" attr-name " isn't present on either consumer/producer contract node!"))
      true) 
    false))

 
(defn schema-rule
  [consumer-contract producer-contract]
  ;;Checks if the class is renamed in the producer schema
  (if (and
       (contains? consumer-contract :$id)
       (= (:$id consumer-contract) (:$id   producer-contract)))
    nil
    {:rule "class is renamed"
     :severity "minor"
     :description (str "consumer node: " consumer-contract
                       " and producer node: " producer-contract
                       " class names are not the same!")}))


(defn attribute-optional
  [consumer-contract producer-contract]
  (when (and (some? (some #{"null"} (:type consumer-contract))) (contains? consumer-contract :type))
    {:rule "attribute-name is optional"
     :severity "minor"
     :description (str "consumer node: " consumer-contract " is optional!")}))


(defn enumeration-values
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :enum))
    (when (contains? consumer-contract :enum)
      (if (< (count (:enum consumer-contract))
             (count (:enum producer-contract))) 
        {:rule "enum values are not same" 
         :severity "major"
         :description (str "consumer node: " consumer-contract 
                           " has less enum values than producer node: "
                           producer-contract)}

        (if (> (count (:enum consumer-contract))
               (count (:enum producer-contract))) 
          {:rule "enum values not same" 
           :severity "minor"
           :description (str "consumer node: " consumer-contract 
                             "has more enum values than producer node: " 
                             producer-contract)}
          
          (when (not= (:enum consumer-contract)
                      (:enum producer-contract))
            {:rule "enum values are not the same"
             :severity "major"
             :description (str "consumer node: " consumer-contract " and producer node: " producer-contract "enum values are not the same")}))))))


(defn string-length
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :maxLength))
    (if  (contains? consumer-contract :maxLength)
      (if (> (:maxLength consumer-contract) (:maxLength producer-contract))
        {:rule "strength length changed" 
         :severity "minor"
         :description (str "consumer node: " consumer-contract 
                           " and producer node: " producer-contract 
                           " maximum string lengths are not the same!") } 
        (when (< (:maxLength consumer-contract) (:maxLength producer-contract))
          {:rule "string length changed" 
           :severity "major"
           :description (str "consumer node: " consumer-contract 
                             " and producer node: " producer-contract 
                             " maximum string lengths are not the same!")})))))


(defn string-pattern
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :pattern))
    (when (and (contains? consumer-contract :pattern)
               (not= (consumer-contract :pattern)
                     (producer-contract :pattern)))
      {:rule "pattern changed" 
       :severity "major"
       :description (str "consumer node: " consumer-contract 
                         " and producer node: " producer-contract 
                         " string patterns are not the same!")})))


(defn numeric-range
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :maximum))
    (when (contains? consumer-contract :maximum)
      (if (> (:maximum consumer-contract)
             (:maximum producer-contract))
        {:rule "numeric range changed" 
         :severity "minor" 
         :description (str "consumer node: " consumer-contract
                           " and producer node:  " producer-contract
                           " numeric ranges aren't the same!")}
        (when (< (:maximum consumer-contract)
                 (:maximum producer-contract) ) 
          {:rule "numeric range changed"
           :severity "major"
           :desciption (str "consumer node: " consumer-contract
                            "and producer node: " producer-contract
                            "numeric range aren't the same")})))))

(defn numeric-precision
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :multipleOf))
    (when (contains? consumer-contract :multipleOf)   
      (if (> (:multipleOf consumer-contract)
             (:multipleOf producer-contract))
        {:rule "numeric precision changed" 
         :severity "minor" 
         :description (str "consumer node: " consumer-contract
                           " and producer node:  " producer-contract
                           " numeric precision aren't the same!")}
        (when (< (:multipleOf consumer-contract)
                 (:multipleOf producer-contract)) 
          {:rule "numeric precision changed"
           :severity "major"
           :desciption (str "consumer node: " consumer-contract
                            "and producer node: " producer-contract
                            "numeric precision  aren't the same")})))))


(defn minimum-cardinality
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :minItems))
    (if (and (contains? consumer-contract :minItems) 
             (< (:minItems consumer-contract)
                (:minItems producer-contract))) 
      {:rule "multiplicity changed" 
       :severity "minor"
       :description (str "consumer node: " consumer-contract
                         " and producer node: " producer-contract
                         " cardinality aren't the same!")} 
      (when (and (contains? consumer-contract :minItems)
                 (> (:minItems consumer-contract)
                    (:minItems producer-contract))) 
        {:rule "multiplicity changed" 
         :severity "major"
         :description (str "consumer node: " consumer-contract
                           " and producer node: " producer-contract
                           " cardinality aren't the same!")}))))


(defn maximum-cardinality
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :maxItems))
    (if (and (contains? consumer-contract :maxItems)
             (> (:maxItems consumer-contract)
                (:maxItems producer-contract))) 
      {:rule "multiplicity changed" 
       :severity "minor"
       :description (str "consumer node: " consumer-contract 
                         " and producer node: " producer-contract
                         " cardinality aren't the same!")} 
      (when (and (contains? consumer-contract :maxItems) 
                 (< (:maxItems consumer-contract)
                    (:maxItems producer-contract))) 
        {:rule "multiplicity changed" 
         :severity "major"
         :description (str "consumer node: " consumer-contract
                           " and producer node: " producer-contract
                           " cardinality aren't the same!")}))))


(defn type-checking
  [consumer-contract producer-contract]
  (if (= false (attr-present consumer-contract producer-contract :type))
    (when (not= (:type consumer-contract)
                (:type producer-contract)) 
      {:rule "attribute type should be the same"
       :severity "major"
       :description (str "consumer node: " consumer-contract " and producer node: " producer-contract " attribute types aren't the same!")})))


(def rules [attribute-optional enumeration-values string-length string-pattern numeric-range numeric-precision minimum-cardinality maximum-cardinality type-checking])

