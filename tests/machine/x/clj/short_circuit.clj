; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:03:23Z
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
;; Function boom takes [a: int, b: int] and returns bool
(defn boom [a b]
  (try
    (_print "boom")
    (throw (ex-info "return" {:value true}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn -main []
  (_print (and false (boom 1 2)))
  (_print (or true (boom 1 2)))
)

(-main)
