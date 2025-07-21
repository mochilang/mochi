(ns main)

(require 'clojure.set)

(defrecord Items [orderId sku])

(defrecord Orders [id customerId])

(defrecord Customers [id name])

(def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"}])

(def orders [{:id 100 :customerId 1} {:id 101 :customerId 2}])

(def items [{:orderId 100 :sku "a"}])

(def result (for [o orders c customers i (let [i_tmp (filter (fn [i] (= (:id o) (:orderId i))) items)] (if (seq i_tmp) i_tmp [nil])) :when (= (:customerId o) (:id c))] {:orderId (:id o) :name (:name c) :item i}))

(defn -main []
  (println "--- Left Join Multi ---")
  (doseq [r result] (println (:orderId r) (:name r) (:item r))))

(-main)
