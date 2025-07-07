(ns main)

(declare m)

(defn -main []
  (def m {1 "a" 2 "b"}) ;; map of int to string
  (println (contains? m 1))
  (println (contains? m 3))
)

(-main)
