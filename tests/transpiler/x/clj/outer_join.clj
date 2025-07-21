(ns main)

(require 'clojure.set)

(defrecord Anon2 [id customerId total])

(defrecord Anon1 [id name])

(def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"} {:id 3 :name "Charlie"} {:id 4 :name "Diana"}])

(def orders [{:id 100 :customerId 1 :total 250} {:id 101 :customerId 2 :total 125} {:id 102 :customerId 1 :total 300} {:id 103 :customerId 5 :total 80}])

(def result (concat (for [o orders :let [c (some (fn [c] (when (= (:customerId o) (:id c)) c)) customers)]] {:order o :customer c}) (for [c customers :when (not-any? (fn [o] (= (:customerId o) (:id c))) orders) :let [o nil]] {:order o :customer c})))

(defn -main []
  (println "--- Outer Join using syntax ---")
  (doseq [row result] (if (:order row) (if (:customer row) (println "Order" (:id (:order row)) "by" (:name (:customer row)) "- $" (:total (:order row))) (println "Order" (:id (:order row)) "by" "Unknown" "- $" (:total (:order row)))) (println "Customer" (:name (:customer row)) "has no orders"))))

(-main)
