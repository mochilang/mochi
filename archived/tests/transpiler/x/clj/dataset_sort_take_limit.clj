(ns main)

(require 'clojure.set)

(defrecord Products [name price])

(def products [{:name "Laptop" :price 1500} {:name "Smartphone" :price 900} {:name "Tablet" :price 600} {:name "Monitor" :price 300} {:name "Keyboard" :price 100} {:name "Mouse" :price 50} {:name "Headphones" :price 200}])

(def expensive (for [p (take 3 (drop 1 (sort-by (fn [p] (- (:price p))) products)))] p))

(defn -main []
  (println "--- Top products (excluding most expensive) ---")
  (doseq [item expensive] (println (:name item) "costs $" (:price item))))

(-main)
