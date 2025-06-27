(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn addTwoNumbers [l1 l2]
  (try
    (def i 0)
    (def j 0)
    (def carry 0)
    (def result [])
    (loop []
      (when (or (or (< i (count l1)) (< j (count l2))) (> carry 0))
        (let [r (try
          (def x 0)
          (when (< i (count l1))
            (def x (_indexList l1 i))
            (def i (+ i 1))
          )
          (def y 0)
          (when (< j (count l2))
            (def y (_indexList l2 j))
            (def j (+ j 1))
          )
          (def sum (+ (+ x y) carry))
          (def digit (mod sum 10))
          (def carry (quot sum 10))
          (def result (vec (concat result [digit])))
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
(throw (ex-info "return" {:value result}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (addTwoNumbers [2 4 3] [5 6 4]) [7 0 8]) "expect failed")
)

(defn test_example_2 []
(assert (= (addTwoNumbers [0] [0]) [0]) "expect failed")
)

(defn test_example_3 []
(assert (= (addTwoNumbers [9 9 9 9 9 9 9] [9 9 9 9]) [8 9 9 9 0 0 0 1]) "expect failed")
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
)

(-main)
