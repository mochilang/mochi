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

(defn test_addition_works []
  (def x (+ 1 2)) ;; int
  (assert (_equal x 3) "expect failed")
)

(defn -main []
  (println "ok")
  (test_addition_works)
)

(-main)
