(ns main)

(require 'clojure.set)

(def people [{:name "Alice" :age 17 :status "minor"} {:name "Bob" :age 25 :status "unknown"} {:name "Charlie" :age 18 :status "unknown"} {:name "Diana" :age 16 :status "minor"}])

(defn -main []
  (def people (vec (map (fn [item] (let [name (:name item) age (:age item) status (:status item)] (if (>= age 18) (assoc (assoc item :status "adult") :age (+ age 1)) item))) people)))
  (println "ok"))

(-main)
