(ns group-by-sort)

(def items
  [{:cat "a" :val 3}
   {:cat "a" :val 1}
   {:cat "b" :val 5}
   {:cat "b" :val 2}])

(def grouped
  (->> items
       (group-by :cat)
       (map (fn [[cat xs]] {:cat cat :total (reduce + (map :val xs))}))
       (sort-by :total >)))

(println grouped)
