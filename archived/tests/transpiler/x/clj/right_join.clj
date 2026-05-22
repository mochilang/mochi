(ns main)

(require 'clojure.set)

(defrecord Orders [id customerId total])

(defrecord Customers [id name])

(def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"} {:id 3 :name "Charlie"} {:id 4 :name "Diana"}])

(def orders [{:id 100 :customerId 1 :total 250} {:id 101 :customerId 2 :total 125} {:id 102 :customerId 1 :total 300}])

(def result (for [o orders c (let [c_tmp (filter (fn [c] (= (:customerId o) (:id c))) customers)] (if (seq c_tmp) c_tmp [nil]))] {:customerName (:name c) :order o}))

(defn -main []
  (println "--- Right Join using syntax ---")
  (doseq [entry result] (if (:order entry) (println "Customer" (:customerName entry) "has order" (:id (:order entry)) "- $" (:total (:order entry))) (println "Customer" (:customerName entry) "has no orders"))))

(-main)
