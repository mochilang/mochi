(ns main)

(require 'clojure.set)

(defrecord People [name age])

(def people [{:name "Alice" :age 30} {:name "Bob" :age 15} {:name "Charlie" :age 65} {:name "Diana" :age 45}])

(def adults (for [person people :when (>= (:age person) 18)] {:name (:name person) :age (:age person) :is_senior (>= (:age person) 60)}))

(defn -main []
  (println "--- Adults ---")
  (doseq [person adults] (println (:name person) "is" (:age person) (if (:is_senior person) " (senior)" ""))))

(-main)
