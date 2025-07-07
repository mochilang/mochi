(ns outer-join)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}
   {:id 3 :name "Charlie"}
   {:id 4 :name "Diana"}])

(def orders
  [{:id 100 :customerId 1 :total 250}
   {:id 101 :customerId 2 :total 125}
   {:id 102 :customerId 1 :total 300}
   {:id 103 :customerId 5 :total 80}])

(def result
  (concat
    (for [o orders]
      (let [c (some #(when (= (:id %) (:customerId o)) %) customers)]
        {:order o :customer c}))
    (for [c customers
          :when (not-any? #(= (:customerId %) (:id c)) orders)]
      {:order nil :customer c})))

(println "--- Outer Join using syntax ---")
(doseq [row result]
  (if (:order row)
    (if (:customer row)
      (println "Order" (get-in row [:order :id]) "by" (get-in row [:customer :name]) "- $" (get-in row [:order :total]))
      (println "Order" (get-in row [:order :id]) "by" "Unknown" "- $" (get-in row [:order :total])))
    (println "Customer" (get-in row [:customer :name]) "has no orders")))
