(ns dataset-sort-take-limit)

(def products
  [{:name "Laptop" :price 1500}
   {:name "Smartphone" :price 900}
   {:name "Tablet" :price 600}
   {:name "Monitor" :price 300}
   {:name "Keyboard" :price 100}
   {:name "Mouse" :price 50}
   {:name "Headphones" :price 200}])

(def expensive
  (->> products
       (sort-by :price >)
       (drop 1)
       (take 3)))

(println "--- Top products (excluding most expensive) ---")
(doseq [item expensive]
  (println (:name item) "costs $" (:price item)))
