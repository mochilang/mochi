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

(declare prefix s1 s2)

(defn -main []
  (def prefix "fore") ;; string
  (def s1 "forest") ;; string
  (println (_equal (subs s1 0 (count prefix)) prefix))
  (def s2 "desert") ;; string
  (println (_equal (subs s2 0 (count prefix)) prefix))
)

(-main)
