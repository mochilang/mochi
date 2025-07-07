(ns left-join)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}])

(def orders
  [{:id 100 :customerId 1 :total 250}
   {:id 101 :customerId 3 :total 80}])

(def result
  (for [o orders]
    (let [c (some #(when (= (:id %) (:customerId o)) %) customers)]
      {:orderId (:id o)
       :customer c
       :total (:total o)})))

(println "--- Left Join ---")
(doseq [entry result]
  (println "Order" (:orderId entry) "customer" (:customer entry) "total" (:total entry)))
