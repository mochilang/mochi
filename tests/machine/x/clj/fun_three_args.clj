(ns main)

;; Function sum3 takes [a: int, b: int, c: int] and returns int
(defn sum3 [a b c]
  (try
    (throw (ex-info "return" {:value (+ (+ a b) c)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (println (sum3 1 2 3))
)

(-main)
