(ns group-by-example)

(def people
  [{:name "Alice" :age 30 :city "Paris"}
   {:name "Bob" :age 15 :city "Hanoi"}
   {:name "Charlie" :age 65 :city "Paris"}
   {:name "Diana" :age 45 :city "Hanoi"}
   {:name "Eve" :age 70 :city "Paris"}
   {:name "Frank" :age 22 :city "Hanoi"}])

(def stats
  (->> people
       (group-by :city)
       (map (fn [[city group]]
              {:city city
               :count (count group)
               :avg_age (/ (reduce + (map :age group))
                           (count group))}))))

(println "--- People grouped by city ---")
(doseq [s stats]
  (println (:city s) ": count =" (:count s) ", avg_age =" (:avg_age s)))
