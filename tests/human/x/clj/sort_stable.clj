(let [items [{:n 1 :v "a"}
             {:n 1 :v "b"}
             {:n 2 :v "c"}]
      result (map :v (sort-by :n items))]
  (println result))
