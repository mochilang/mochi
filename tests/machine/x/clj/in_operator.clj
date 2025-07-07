(ns main)

(declare xs)

(defn -main []
  (def xs [1 2 3]) ;; list of int
  (println (some #(= 2 %) xs))
  (println (not (some #(= 5 %) xs)))
)

(-main)
