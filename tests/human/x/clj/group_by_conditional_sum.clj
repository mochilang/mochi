(ns group-by-conditional-sum)

(def items
  [{:cat "a" :val 10 :flag true}
   {:cat "a" :val 5 :flag false}
   {:cat "b" :val 20 :flag true}])

(def result
  (->> items
       (group-by :cat)
       (map (fn [[cat xs]]
              (let [num (reduce + (map (fn [x] (if (:flag x) (:val x) 0)) xs))
                    den (reduce + (map :val xs))]
                {:cat cat
                 :share (/ num den)})))
       (sort-by :cat)))

(println result)
