(ns main)

(declare m)

(defn -main []
  (def m {1 "a" 2 "b"}) ;; map of int to string
  (println (get m 1))
)

(-main)
