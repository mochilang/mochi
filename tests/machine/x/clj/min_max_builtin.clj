(ns main)

(declare nums)

(defn -main []
  (def nums [3 1 4]) ;; list of int
  (println (apply min nums))
  (println (apply max nums))
)

(-main)
