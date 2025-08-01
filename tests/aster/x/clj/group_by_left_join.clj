(ns main)
(require 'clojure.set)
(defrecord Orders [id customerId])
(defrecord Customers [id name])
(def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"} {:id 3 :name "Charlie"}])
(def orders [{:id 100 :customerId 1} {:id 101 :customerId 1} {:id 102 :customerId 2}])
(def stats (for [g (for [[k rows] (group-by :key (for [c customers o (let [o_tmp (filter (fn [o] (= (:customerId o) (:id c))) orders)] (if (seq o_tmp) o_tmp [nil])) :let [k (:name c)]] {:key k :item {:c c :o o}})) :let [g {:key k :items (map :item rows)}]] g)] {:name (:key g) :count (count (for [r (:items g) :when (:o r)] r))}))
(defn -main [] (println "--- Group Left Join ---") (doseq [s stats] (println (:name s) "orders:" (:count s))))
(-main)
