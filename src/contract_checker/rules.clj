(ns contract-checker.rules
  (:require [clojure.data.json      :as json])
  (:refer-clojure :exclude [contains?]))


(defn contains?
  [coll item]
 (cond
   (map? coll)  (clojure.core/contains? coll item)
   (sequential? coll) (some? (some #{item} coll))
   :default      false))


(defn- attr-in-both?
  [consumer-contract producer-contract attr-name]
  (if (and (contains? consumer-contract attr-name)
           (contains? producer-contract attr-name))
    true
    false))


(defn keys-same-rule
  [consumer-contract producer-contract]
  (let [ consumer-keys ( into #{} (keys consumer-contract))
        producer-keys (into #{} (keys producer-contract)) ] 
    (when (not ( = consumer-keys producer-keys))
      {:rule "attributes mismatch"
       :severity "major"
       :description (str "consumer node attributes: " consumer-keys
                         " and producer node attributes: " producer-keys
                         " aren't the same!")})))


(defn class-renamed-rule
  [consumer-contract producer-contract]
  ;;returns the impact when the class is renamed in the producer schema
  (if (and
       (attr-in-both? consumer-contract producer-contract :$id)
       (= (:$id consumer-contract) (:$id   producer-contract)))
    nil
    {:rule "class is renamed"
     :severity "minor"
     :description (str "consumer node: " consumer-contract
                       " and producer node: " producer-contract
                       " class names are not the same!")}))


(defn attribute-optional-rule
  [consumer-contract]
  ;;checks if the attribute is optional in the consumer contract
  (when (and (some? (some #{"null"} (:type consumer-contract))) (contains? consumer-contract :type))
    {:rule "attribute-name is optional"
     :severity "minor"
     :description (str "consumer node: " consumer-contract " is optional!")}))


(defn enumeration-values-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when an enumeration value is added/removed or the enumeration values aren't same
  (if (attr-in-both? consumer-contract producer-contract :enum)
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
           :description (str "consumer node: " consumer-contract " and producer node: " producer-contract "enum values are not the same")})))))


(defn string-length-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when the string length is decreased or increased
  (if (attr-in-both? consumer-contract producer-contract :maxLength)
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
                           " maximum string lengths are not the same!")}))))


(defn string-pattern-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when the string pattern is modified
  (if (attr-in-both? consumer-contract producer-contract :pattern)
    (when
      (not= (consumer-contract :pattern)
            (producer-contract :pattern))
      {:rule "pattern changed" 
       :severity "major"
       :description (str "consumer node: " consumer-contract 
                         " and producer node: " producer-contract 
                         " string patterns are not the same!")})))


(defn numeric-range-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when the numeric range cintracts or expands
  (if (attr-in-both? consumer-contract producer-contract :maximum)
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
                          "numeric range aren't the same")}))))

(defn numeric-precision-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when the numeric precison for a number data type decreases or increases
  (if (attr-in-both? consumer-contract producer-contract :multipleOf)   
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
                          "numeric precision  aren't the same")}))))


(defn minimum-cardinality-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when the minimum cardinality of an association decreases or increases
  (if (attr-in-both? consumer-contract producer-contract :minItems)
    (if (< (:minItems consumer-contract)
           (:minItems producer-contract)) 
      {:rule "multiplicity changed" 
       :severity "minor"
       :description (str "consumer node: " consumer-contract
                         " and producer node: " producer-contract
                         " cardinality aren't the same!")} 
      (when (and (attr-in-both? consumer-contract producer-contract :minItems)
                 (> (:minItems consumer-contract)
                    (:minItems producer-contract))) 
        {:rule "multiplicity changed" 
         :severity "major"
         :description (str "consumer node: " consumer-contract
                           " and producer node: " producer-contract
                           " cardinality aren't the same!")}))))


(defn maximum-cardinality-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when the maximum cardinality of an association decreases or increases
  (if (attr-in-both? consumer-contract producer-contract :maxItems)
    (if (> (:maxItems consumer-contract)
           (:maxItems producer-contract)) 
      {:rule "multiplicity changed" 
       :severity "minor"
       :description (str "consumer node: " consumer-contract 
                         " and producer node: " producer-contract
                         " cardinality aren't the same!")} 
      (when (and (attr-in-both? consumer-contract producer-contract :maxItems) 
                 (< (:maxItems consumer-contract)
                    (:maxItems producer-contract))) 
        {:rule "multiplicity changed" 
         :severity "major"
         :description (str "consumer node: " consumer-contract
                           " and producer node: " producer-contract
                           " cardinality aren't the same!")}))))


(defn type-checking-rule
  [consumer-contract producer-contract]
  ;;returns the impact on the consumer when the type attribute changes
  (if (attr-in-both? consumer-contract producer-contract :type)
    (when (not= (:type consumer-contract)
                (:type producer-contract)) 
      {:rule "attribute type should be the same"
       :severity "major"
       :description (str "consumer node: " consumer-contract " and producer node: " producer-contract " attribute types aren't the same!")})))


(def rules [class-renamed-rule attribute-optional-rule enumeration-values-rule string-length-rule string-pattern-rule numeric-range-rule numeric-precision-rule minimum-cardinality-rule maximum-cardinality-rule type-checking-rule keys-same-rule])

