(defn reverse [x]
  (try
    (def sign 1)
    (def n x)
    (when (< n 0)
      (def sign (- 1))
      (def n (- n))
    )
    (def rev 0)
    (loop []
      (when (not= n 0)
        (let [r (try
          (def digit (mod n 10))
          (def rev (+ (* rev 10) digit))
          (def n (quot n 10))
          :next
        (catch clojure.lang.ExceptionInfo e
          (cond
            (= (.getMessage e) "continue") :next
            (= (.getMessage e) "break") :break
            :else (throw e))
          )
        )]
      (cond
        (= r :break) nil
        (= r :next) (recur)
      )
    )
  )
)
(def rev (* rev sign))
(when (or (< rev (- (- 2147483647) 1)) (> rev 2147483647))
  (throw (ex-info "return" {:value 0}))
)
(throw (ex-info "return" {:value rev}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (reverse 123) 321))
)

(defn test_example_2 []
(assert (= (reverse (- 123)) (- 321)))
)

(defn test_example_3 []
(assert (= (reverse 120) 21))
)

(defn test_overflow []
(assert (= (reverse 1534236469) 0))
)

(test_example_1)
(test_example_2)
(test_example_3)
(test_overflow)
