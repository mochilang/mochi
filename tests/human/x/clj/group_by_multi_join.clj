(ns group-by-multi-join)

(def nations
  [{:id 1 :name "A"}
   {:id 2 :name "B"}])

(def suppliers
  [{:id 1 :nation 1}
   {:id 2 :nation 2}])

(def partsupp
  [{:part 100 :supplier 1 :cost 10.0 :qty 2}
   {:part 100 :supplier 2 :cost 20.0 :qty 1}
   {:part 200 :supplier 1 :cost 5.0 :qty 3}])

(def filtered
  (for [ps partsupp
        s suppliers :when (= (:id s) (:supplier ps))
        n nations :when (= (:id n) (:nation s))
        :when (= (:name n) "A")]
    {:part (:part ps)
     :value (* (:cost ps) (:qty ps))}))

(def grouped
  (->> filtered
       (group-by :part)
       (map (fn [[p xs]] {:part p :total (reduce + (map :value xs))}))))

(println grouped)
