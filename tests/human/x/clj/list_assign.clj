(let [nums (atom [1 2])]
  (swap! nums assoc 1 3)
  (println (nth @nums 1)))
