; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:01:52Z
(ns main)

(defn _print [& args]
  (letfn [(pv [v]
            (cond
              (true? v) (print 1)
              (false? v) (print 0)
              (sequential? v) (doseq [[i x] (map-indexed vector v)]
                                (when (> i 0) (print " "))
                                (pv x))
              :else (print v)))]
    (doseq [[i a] (map-indexed vector args)]
      (when (> i 0) (print " "))
      (pv a))
    (println)))
(declare add10)

;; Function makeAdder takes [n: int] and returns function
(defn makeAdder [n]
  (try
    (throw (ex-info "return" {:value (fn [x]
  (try
    (throw (ex-info "return" {:value (+ x n)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (def add10 (makeAdder 10)) ;; function
  (_print (add10 7))
)

(-main)
