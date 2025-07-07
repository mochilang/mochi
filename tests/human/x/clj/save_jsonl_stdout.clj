(ns save-jsonl-stdout
  (:require [clojure.data.json :as json]))

(def people
  [{:name "Alice" :age 30}
   {:name "Bob" :age 25}])

(doseq [p people]
  (println (json/write-str p)))
