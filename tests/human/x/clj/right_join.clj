(ns right-join)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}
   {:id 3 :name "Charlie"}
   {:id 4 :name "Diana"}])

(def orders
  [{:id 100 :customerId 1 :total 250}
   {:id 101 :customerId 2 :total 125}
   {:id 102 :customerId 1 :total 300}])

(def result
  (for [c customers]
    {:customerName (:name c)
     :order (some #(when (= (:customerId %) (:id c)) %) orders)}))

(println "--- Right Join using syntax ---")
(doseq [entry result]
  (if (:order entry)
    (println "Customer" (:customerName entry) "has order" (get-in entry [:order :id]) "- $" (get-in entry [:order :total]))
    (println "Customer" (:customerName entry) "has no orders")))
