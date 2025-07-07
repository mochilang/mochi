(ns inner-join)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}
   {:id 3 :name "Charlie"}])

(def orders
  [{:id 100 :customerId 1 :total 250}
   {:id 101 :customerId 2 :total 125}
   {:id 102 :customerId 1 :total 300}
   {:id 103 :customerId 4 :total 80}])

(def result
  (for [o orders
        c customers
        :when (= (:customerId o) (:id c))]
    {:orderId (:id o)
     :customerName (:name c)
     :total (:total o)}))

(println "--- Orders with customer info ---")
(doseq [entry result]
  (println "Order" (:orderId entry) "by" (:customerName entry) "- $" (:total entry)))
