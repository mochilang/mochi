(ns group-items-iteration)

(def data
  [{:tag "a" :val 1}
   {:tag "a" :val 2}
   {:tag "b" :val 3}])

(def result
  (->> data
       (group-by :tag)
       (map (fn [[tag xs]]
              {:tag tag
               :total (reduce + (map :val xs))}))
       (sort-by :tag)))

(println result)
