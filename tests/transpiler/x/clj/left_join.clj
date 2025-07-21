(ns main)

(require 'clojure.set)

(defrecord Orders [id customerId total])

(defrecord Customers [id name])

(def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"}])

(def orders [{:id 100 :customerId 1 :total 250} {:id 101 :customerId 3 :total 80}])

(def result (for [o orders c (let [c_tmp (filter (fn [c] (= (:customerId o) (:id c))) customers)] (if (seq c_tmp) c_tmp [nil]))] {:orderId (:id o) :customer c :total (:total o)}))

(defn -main []
  (println "--- Left Join ---")
  (doseq [entry result] (println "Order" (:orderId entry) "customer" (:customer entry) "total" (:total entry))))

(-main)
