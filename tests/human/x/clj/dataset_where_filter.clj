(ns dataset-where-filter)

(def people
  [{:name "Alice" :age 30}
   {:name "Bob" :age 15}
   {:name "Charlie" :age 65}
   {:name "Diana" :age 45}])

(def adults
  (map (fn [p]
         {:name (:name p)
          :age (:age p)
          :is_senior (>= (:age p) 60)})
       (filter #(>= (:age %) 18) people)))

(println "--- Adults ---")
(doseq [person adults]
  (let [suffix (if (:is_senior person) " (senior)" "")] 
    (println (:name person) "is" (:age person) suffix)))
