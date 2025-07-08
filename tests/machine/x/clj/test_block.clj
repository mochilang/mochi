(ns main)

(defn test_addition_works []
  (def x (+ 1 2)) ;; int
  (assert (= x 3) "expect failed")
)

(defn -main []
  (println "ok")
  (test_addition_works)
)

(-main)
