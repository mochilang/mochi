(ns main)

(declare c)

(defn Counter [n]
  {:__name "Counter" :n n}
)


;; Function inc takes [c: Counter] and returns any
(defn inc [c]
  (try
    (def c (+ (:n c) 1)) ;; int
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (def c {:__name "Counter" :n 0}) ;; Counter
  (inc c)
  (println (:n c))
)

(-main)
