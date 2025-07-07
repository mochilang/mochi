(ns main)

;; Function sum_rec takes [n: int, acc: int] and returns int
(defn sum_rec [n acc]
  (try
    (when (= n 0)
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
