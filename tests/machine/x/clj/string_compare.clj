(ns main)

(defn -main []
  (println (< (compare "a" "b") 0))
  (println (<= (compare "a" "a") 0))
  (println (> (compare "b" "a") 0))
  (println (>= (compare "b" "b") 0))
)

(-main)
