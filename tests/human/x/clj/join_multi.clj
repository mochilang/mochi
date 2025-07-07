(ns join-multi)

(def customers
  [{:id 1 :name "Alice"}
   {:id 2 :name "Bob"}])

(def orders
  [{:id 100 :customerId 1}
   {:id 101 :customerId 2}])

(def items
  [{:orderId 100 :sku "a"}
   {:orderId 101 :sku "b"}])

(def result
  (for [o orders
        c customers :when (= (:customerId o) (:id c))
        i items :when (= (:orderId i) (:id o))]
    {:name (:name c) :sku (:sku i)}))

(println "--- Multi Join ---")
(doseq [r result]
  (println (:name r) "bought item" (:sku r)))
