(ns contract-checker.aws
  (:require [clojure.data.json      :as json]
            [contract-checker.core  :as cc]
            [clojure.java.io        :as io]))


(import com.amazonaws.services.lambda.runtime.Context)


(defn conform-contract
  [js]
  (let [in (json/read-str js :key-fn keyword)
        consumer (:consumer in)
        producer (:producer in)]
    (if (and consumer producer)
      (json/write-str {:errors (cc/check-contract (:consumer in) (:producer in))})
      (json/write-str {:errors "You must specify both a consumer and producer contract!"}))))


(gen-class
 :name clojurehandler.Handler
 :prefix aws-
 :implements [com.amazonaws.services.lambda.runtime.RequestStreamHandler])


(defn aws-handleRequest [this input output ctx]
  (let [out (-> (slurp input)
                 conform-contract)]
    (with-open [o (io/writer output)]
      (.write o out))))
