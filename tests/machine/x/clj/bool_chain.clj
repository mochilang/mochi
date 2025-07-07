(ns main)

;; Function boom returns bool
(defn boom []
  (try
    (println "boom")
    (throw (ex-info "return" {:value true}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (println (and (and (< 1 2) (< 2 3)) (< 3 4)))
  (println (and (and (< 1 2) (> 2 3)) (boom )))
  (println (and (and (and (< 1 2) (< 2 3)) (> 3 4)) (boom )))
)

(-main)
