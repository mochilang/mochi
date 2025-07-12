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

;; Function sum_rec takes [n: int, acc: int] and returns int
(defn sum_rec [n acc]
  (try
    (when (_equal n 0)
      (throw (ex-info "return" {:value acc}))
    )
    (throw (ex-info "return" {:value (sum_rec (- n 1) (+ acc n))}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (println (sum_rec 10 0))
)

(-main)
