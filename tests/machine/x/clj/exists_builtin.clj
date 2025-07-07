(ns main)

(declare data flag)

(defn -main []
  (def data [1 2]) ;; list of int
  (def flag (exists (vec (->> (for [x data :when (= x 1)] x))))) ;; bool
  (println flag)
)

(-main)
