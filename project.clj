(defproject contract-checker "0.1.0"
  :description "A Clojure library for checking whether consumer data contracts defined in json-schema are compatible with producer contracts."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "0.2.7"]
                 [com.amazonaws/aws-lambda-java-core "1.1.0"]
                 [rhizome "0.2.9"]]
  :repl-options {:init-ns contract-checker.core}
  :aot :all)
