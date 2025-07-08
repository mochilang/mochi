(ns main)

;; Function outer takes [x: int] and returns int
(defn outer [x]
  (try
    ;; Function inner takes [y: int] and returns int
    (defn inner [y]
      (try
        (throw (ex-info "return" {:value (+ x y)}))
      (catch clojure.lang.ExceptionInfo e
        (if (= (.getMessage e) "return")
          (:value (ex-data e))
        (throw e)))
      )
    )
    (throw (ex-info "return" {:value (inner 5)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (println (outer 3))
)

(-main)
