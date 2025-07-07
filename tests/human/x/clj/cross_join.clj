(ns cross-join)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}
   {:id 3 :name "Charlie"}])

(def orders
  [{:id 100 :customerId 1 :total 250}
   {:id 101 :customerId 2 :total 125}
   {:id 102 :customerId 1 :total 300}])

(def result
  (for [o orders
        c customers]
    {:orderId (:id o)
     :orderCustomerId (:customerId o)
     :pairedCustomerName (:name c)
     :orderTotal (:total o)}))

(println "--- Cross Join: All order-customer pairs ---")
(doseq [entry result]
  (println "Order" (:orderId entry)
           "(customerId:" (:orderCustomerId entry)
           ", total: $" (:orderTotal entry)
           ") paired with" (:pairedCustomerName entry)))
