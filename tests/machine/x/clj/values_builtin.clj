(ns main)

(declare m)

(defn -main []
  (def m {"a" 1 "b" 2 "c" 3}) ;; map of string to int
  (println (vec (vals m)))
)

(-main)
