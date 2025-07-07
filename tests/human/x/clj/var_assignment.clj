(let [x (atom 1)]
  (reset! x 2)
  (println @x))
