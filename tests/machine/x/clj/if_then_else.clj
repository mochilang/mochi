(ns main)

(declare x msg)

(defn -main []
  (def x 12) ;; int
  (def msg (if (> x 10) "yes" "no")) ;; string
  (println msg)
)

(-main)
