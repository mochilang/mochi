(let [data (atom {:outer {:inner 1}})]
  (swap! data assoc-in [:outer :inner] 2)
  (println (get-in @data [:outer :inner])))
