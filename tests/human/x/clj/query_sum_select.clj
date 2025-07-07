(let [nums [1 2 3]
      result (apply + (filter #(> % 1) nums))]
  (println result))
