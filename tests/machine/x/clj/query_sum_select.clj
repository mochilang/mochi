(ns main)

(defn _sum [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "sum() expects list or group" {})))]
    (reduce + 0 lst))
  )

(declare nums result)

(defn -main []
  (def nums [1 2 3]) ;; list of int
  (def result (_sum (vec (->> (for [n nums :when (> n 1)] n))))) ;; list of int
  (println result)
)

(-main)
