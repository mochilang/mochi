(let [scores (atom {"alice" 1})]
  (swap! scores assoc "bob" 2)
  (println (@scores "bob")))
