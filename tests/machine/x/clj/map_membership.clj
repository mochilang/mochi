(ns main)

(declare m)

(defn -main []
  (def m {"a" 1 "b" 2}) ;; map of string to int
  (println (contains? m "a"))
  (println (contains? m "c"))
)

(-main)
