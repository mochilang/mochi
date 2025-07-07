(loop [i 0]
  (when (< i 3)
    (println i)
    (recur (inc i))))
