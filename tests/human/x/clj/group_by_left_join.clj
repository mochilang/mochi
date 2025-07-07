(ns group-by-left-join)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}
   {:id 3 :name "Charlie"}])

(def orders
  [{:id 100 :customerId 1}
   {:id 101 :customerId 1}
   {:id 102 :customerId 2}])

(def stats
  (for [c customers]
    {:name (:name c)
     :count (count (filter #(= (:customerId %) (:id c)) orders))}))

(println "--- Group Left Join ---")
(doseq [s stats]
  (println (:name s) "orders:" (:count s)))
