(ns main)

(declare items result)

(defn -main []
  (def items [{:n 1 :v "a"} {:n 1 :v "b"} {:n 2 :v "c"}]) ;; list of map of string to any
  (def result (vec (->> (for [i items] (:v i)) (sort-by (fn [i] (:n i)))))) ;; list of any
  (println result)
)

(-main)
