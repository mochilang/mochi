(ns main)

(declare x y m)

(defn -main []
  (def x 3) ;; int
  (def y 4) ;; int
  (def m {"a" x "b" y}) ;; map of string to int
  (println (get m "a") (get m "b"))
)

(-main)
