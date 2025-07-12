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

(declare a b)

(defn -main []
  (def a (- 10 3)) ;; int
  (def b (+ 2 2)) ;; int
  (println a)
  (println (_equal a 7))
  (println (< b 5))
)

(-main)
