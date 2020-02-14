(ns contract-checker.rules
  (:require [clojure.data.json      :as json])
  (:refer-clojure :exclude [contains?])
  (:require [clojure.set :as set]))


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
  [consumer-contract producer-contract path]
  (let [ consumer-keys ( into #{} (keys consumer-contract))
        producer-keys (into #{} (keys producer-contract)) 
        errors (for [x (set/difference consumer-keys producer-keys)]
                  {:rule "Attributes mismatch"
                   :severity "Major"
                   :description (str "Consumer node attributes: " consumer-keys
                                     " and producer node attributes: " producer-keys
                                     " aren't the same!")
                   :path (conj path x)})]
    errors))


(defn class-renamed-rule
  [consumer-contract producer-contract path]
  ;;returns the impact when the class is renamed in the producer schema
  (when (and
       (attr-in-both? consumer-contract producer-contract :$id)
       (not= (:$id consumer-contract) (:$id   producer-contract)))
    {:rule "Class is renamed"
     :severity "Minor"
     :description (str "Consumer node: " consumer-contract
                       " and producer node: " producer-contract
                       " class names are not the same!")
     :path (conj path :$id)}))


(defn attribute-optional-rule
  [consumer-contract producer-contract path]
  ;;checks if the attribute is optional in the consumer contract
  (when (and (some? (some #{"null"} (:type consumer-contract))) (contains? consumer-contract :type))
    {:rule "Attribute name is optional"
     :severity "Minor"
     :description (str "Consumer node: " consumer-contract " is optional!")
     :path (conj path :type)}))


(defn enumeration-values-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when an enumeration value is added/removed or the enumeration values aren't same
  (if (attr-in-both? consumer-contract producer-contract :enum)
    (if (< (count (:enum consumer-contract))
           (count (:enum producer-contract))) 
      {:rule "Enum values are not same" 
       :severity "Major"
       :description (str "Consumer node: " consumer-contract 
                         " has less enum values than producer node: "
                         producer-contract)
        :path (conj path :enum)}

      (if (> (count (:enum consumer-contract))
             (count (:enum producer-contract))) 
        {:rule "Enum values not same" 
         :severity "Minor"
         :description (str "Consumer node: " consumer-contract 
                           "has more enum values than producer node: " 
                           producer-contract)
          :path (conj path :enum)}
        
        (when (not= (:enum consumer-contract)
                    (:enum producer-contract))
          {:rule "Enum values are not the same"
           :severity "Major"
           :description (str "Consumer node: " consumer-contract " and producer node: " producer-contract "enum values are not the same")
            :path (conj path :enum)})))))


(defn string-length-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when the string length is decreased or increased
  (if (attr-in-both? consumer-contract producer-contract :maxLength)
    (if (> (:maxLength consumer-contract) (:maxLength producer-contract))
      {:rule "Strength length changed" 
       :severity "Minor"
       :description (str "Consumer node: " consumer-contract 
                         " and producer node: " producer-contract 
                         " maximum string lengths are not the same!")
        :path (conj path :maxLength)} 
      (when (< (:maxLength consumer-contract) (:maxLength producer-contract))
        {:rule "String length changed" 
         :severity "Major"
         :description (str "Consumer node: " consumer-contract 
                           " and producer node: " producer-contract 
                           " maximum string lengths are not the same!")
          :path (conj path :maxLength)}))))


(defn string-pattern-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when the string pattern is modified
  (if (attr-in-both? consumer-contract producer-contract :pattern)
    (when
      (not= (consumer-contract :pattern)
            (producer-contract :pattern))
      {:rule "Pattern changed" 
       :severity "Major"
       :description (str "Consumer node: " consumer-contract 
                         " and producer node: " producer-contract 
                         " string patterns are not the same!")
        :path (conj path :pattern)})))


(defn numeric-range-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when the numeric range cintracts or expands
  (if (attr-in-both? consumer-contract producer-contract :maximum)
    (if (> (:maximum consumer-contract)
           (:maximum producer-contract))
      {:rule "Numeric range changed" 
       :severity "Minor" 
       :description (str "Consumer node: " consumer-contract
                         " and producer node:  " producer-contract
                         " numeric ranges aren't the same!")
        :path (conj path :maximum)}
      (when (< (:maximum consumer-contract)
               (:maximum producer-contract) ) 
        {:rule "Numeric range changed"
         :severity "Major"
         :desciption (str "Consumer node: " consumer-contract
                          "and producer node: " producer-contract
                          "numeric range aren't the same")
          :path (conj path :maximum)}))))


(defn numeric-precision-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when the numeric precison for a number data type decreases or increases
  (if (attr-in-both? consumer-contract producer-contract :multipleOf)   
    (if (> (:multipleOf consumer-contract)
           (:multipleOf producer-contract))
      {:rule "Numeric precision changed" 
       :severity "Minor" 
       :description (str "Consumer node: " consumer-contract
                         " and producer node:  " producer-contract
                         " numeric precision aren't the same!")
        :path (conj path :multipleOf)}
      (when (< (:multipleOf consumer-contract)
               (:multipleOf producer-contract)) 
        {:rule "Numeric precision changed"
         :severity "Major"
         :desciption (str "Consumer node: " consumer-contract
                          "and producer node: " producer-contract
                          "numeric precision  aren't the same")
          :path (conj path :multipleOf)}))))


(defn minimum-cardinality-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when the minimum cardinality of an association decreases or increases
  (if (attr-in-both? consumer-contract producer-contract :minItems)
    (if (< (:minItems consumer-contract)
           (:minItems producer-contract)) 
      {:rule "Multiplicity changed" 
       :severity "Minor"
       :description (str "Consumer node: " consumer-contract
                         " and producer node: " producer-contract
                         " cardinality aren't the same!")
        :path (conj path :minItems)} 
      (when (and (attr-in-both? consumer-contract producer-contract :minItems)
                 (> (:minItems consumer-contract)
                    (:minItems producer-contract))) 
        {:rule "Multiplicity changed" 
         :severity "Major"
         :description (str "Consumer node: " consumer-contract
                           " and producer node: " producer-contract
                           " cardinality aren't the same!")
          :path (conj path :minItems)}))))


(defn maximum-cardinality-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when the maximum cardinality of an association decreases or increases
  (if (attr-in-both? consumer-contract producer-contract :maxItems)
    (if (> (:maxItems consumer-contract)
           (:maxItems producer-contract)) 
      {:rule "Multiplicity changed" 
       :severity "Minor"
       :description (str "Consumer node: " consumer-contract 
                         " and producer node: " producer-contract
                         " cardinality aren't the same!")
        :path (conj path :maxItems)} 
      (when (and (attr-in-both? consumer-contract producer-contract :maxItems) 
                 (< (:maxItems consumer-contract)
                    (:maxItems producer-contract))) 
        {:rule "Multiplicity changed" 
         :severity "Major"
         :description (str "Consumer node: " consumer-contract
                           " and producer node: " producer-contract
                           " cardinality aren't the same!")
          :path (conj path :maxItems)}))))


(defn type-checking-rule
  [consumer-contract producer-contract path]
  ;;returns the impact on the consumer when the type attribute changes
  (if (attr-in-both? consumer-contract producer-contract :type)
    (when (not= (:type consumer-contract)
                (:type producer-contract)) 
      {:rule "Attribute type should be the same"
       :severity "Major"
       :description (str "Consumer node: " consumer-contract " and producer node: " producer-contract " attribute types aren't the same!")
       :path (conj path :type)})))


(def rules [class-renamed-rule attribute-optional-rule enumeration-values-rule string-length-rule string-pattern-rule numeric-range-rule numeric-precision-rule minimum-cardinality-rule maximum-cardinality-rule type-checking-rule keys-same-rule])

