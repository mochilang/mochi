(ns main)

(declare a)

(defn -main []
  (def a [1 2]) ;; list of int
  (println (conj a 3))
)

(-main)
