(ns order-by-map)

(def data
  [{:a 1 :b 2}
   {:a 1 :b 1}
   {:a 0 :b 5}])

(def sorted
  (sort-by (juxt :a :b) data))

(println sorted)
