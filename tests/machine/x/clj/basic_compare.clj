(ns main)

(declare a b)

(defn -main []
  (def a (- 10 3)) ;; int
  (def b (+ 2 2)) ;; int
  (println a)
  (println (= a 7))
  (println (< b 5))
)

(-main)
