(ns main)

(declare k)

;; Function inc takes [x: int] and returns int
(defn inc [x]
  (try
    (throw (ex-info "return" {:value (+ x k)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (def k 2) ;; int
  (println (inc 3))
)

(-main)
