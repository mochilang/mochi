(let [matrix (atom [[1 2] [3 4]])]
  (swap! matrix assoc-in [1 0] 5)
  (println (get-in @matrix [1 0])))
