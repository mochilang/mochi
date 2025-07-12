(ns main)

(defn _equal [a b]
  (cond
    (and (sequential? a) (sequential? b))
      (and (= (count a) (count b)) (every? true? (map _equal a b)))
    (and (map? a) (map? b))
      (and (= (count a) (count b))
           (every? (fn [k] (_equal (get a k) (get b k))) (keys a)))
    (and (number? a) (number? b))
      (= (double a) (double b))
    :else
      (= a b)))

(declare data flag)

(defn -main []
  (def data [1 2]) ;; list of int
  (def flag (boolean (seq (vec (->> (for [x data :when (_equal x 1)] x)))))) ;; bool
  (println flag)
)

(-main)
