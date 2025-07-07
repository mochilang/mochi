(ns main)

(declare x msg)

(defn -main []
  (def x 8) ;; int
  (def msg (if (> x 10) "big" (if (> x 5) "medium" "small"))) ;; string
  (println msg)
)

(-main)
