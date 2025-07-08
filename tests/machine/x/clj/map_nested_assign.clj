(ns main)

(declare data)

(defn -main []
  (def data {"outer" {"inner" 1}}) ;; map of string to map of string to int
  (def data (assoc-in data ["outer" "inner"] 2)) ;; int
  (println (get (get data "outer") "inner"))
)

(-main)
