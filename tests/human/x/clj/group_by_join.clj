(ns group-by-join)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}])

(def orders
  [{:id 100 :customerId 1}
   {:id 101 :customerId 1}
   {:id 102 :customerId 2}])

(def stats
  (->> orders
       (map (fn [o]
              (let [c (some #(when (= (:id %) (:customerId o)) %) customers)]
                (:name c))))
       (group-by identity)
       (map (fn [[name grp]] {:name name :count (count grp)}))))

(println "--- Orders per customer ---")
(doseq [s stats]
  (println (:name s) "orders:" (:count s)))
