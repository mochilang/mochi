(ns main)

(defn -main []
  (println (let [xs [1 2 3]] (if (empty? xs) 0 (/ (reduce + xs) (double (count xs))))))
)

(-main)
