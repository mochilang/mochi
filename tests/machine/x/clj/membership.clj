(ns main)

(declare nums)

(defn -main []
  (def nums [1 2 3]) ;; list of int
  (println (some #(= 2 %) nums))
  (println (some #(= 4 %) nums))
)

(-main)
