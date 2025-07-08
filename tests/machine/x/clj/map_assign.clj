(ns main)

(declare scores)

(defn -main []
  (def scores {"alice" 1}) ;; map of string to int
  (def scores (assoc scores "bob" 2)) ;; int
  (println (get scores "bob"))
)

(-main)
