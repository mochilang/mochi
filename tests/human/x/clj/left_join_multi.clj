(ns left-join-multi)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}])

(def orders
  [{:id 100 :customerId 1}
   {:id 101 :customerId 2}])

(def items
  [{:orderId 100 :sku "a"}])

(def result
  (for [o orders
        c customers :when (= (:customerId o) (:id c))]
    {:orderId (:id o)
     :name (:name c)
     :item (some #(when (= (:orderId %) (:id o)) %) items)}))

(println "--- Left Join Multi ---")
(doseq [r result]
  (println (:orderId r) (:name r) (:item r)))
