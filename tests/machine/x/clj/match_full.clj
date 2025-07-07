(ns main)

(declare x label day mood ok status)

;; Function classify takes [n: int] and returns string
(defn classify [n]
  (try
    (throw (ex-info "return" {:value (let [t n]
  (cond
    (= t 0) "zero"
    (= t 1) "one"
    :else "many"
  ))}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (def x 2) ;; int
  (def label (let [t x]
  (cond
    (= t 1) "one"
    (= t 2) "two"
    (= t 3) "three"
    :else "unknown"
  ))) ;; string
  (println label)
  (def day "sun") ;; string
  (def mood (let [t day]
  (cond
    (= t "mon") "tired"
    (= t "fri") "excited"
    (= t "sun") "relaxed"
    :else "normal"
  ))) ;; string
  (println mood)
  (def ok true) ;; bool
  (def status (let [t ok]
  (cond
    (= t true) "confirmed"
    (= t false) "denied"
    :else nil
  ))) ;; string
  (println status)
  (println (classify 0))
  (println (classify 5))
)

(-main)
