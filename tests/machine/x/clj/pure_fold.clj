(ns main)

;; Function triple takes [x: int] and returns int
(defn triple [x]
  (try
    (throw (ex-info "return" {:value (* x 3)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (println (triple (+ 1 2)))
)

(-main)
