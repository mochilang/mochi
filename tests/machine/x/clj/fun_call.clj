(ns main)

;; Function add takes [a: int, b: int] and returns int
(defn add [a b]
  (try
    (throw (ex-info "return" {:value (+ a b)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (println (add 2 3))
)

(-main)
