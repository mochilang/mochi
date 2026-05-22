(ns main)

(require 'clojure.set)

(defrecord Items [orderId sku])

(defrecord Orders [id customerId])

(defrecord Customers [id name])

(def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"}])

(def orders [{:id 100 :customerId 1} {:id 101 :customerId 2}])

(def items [{:orderId 100 :sku "a"} {:orderId 101 :sku "b"}])

(def result (for [o orders c customers i items :when (and (= (:customerId o) (:id c)) (= (:id o) (:orderId i)))] {:name (:name c) :sku (:sku i)}))

(defn -main []
  (println "--- Multi Join ---")
  (doseq [r result] (println (:name r) "bought item" (:sku r))))

(-main)
