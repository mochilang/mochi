(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn removeNthFromEnd [nums n]
  (try
    (def idx (- (count nums) n))
    (def result [])
    (def i 0)
    (loop []
      (when (< i (count nums))
        (let [r (try
          (when (not= i idx)
            (def result (vec (concat result [(_indexList nums i)])))
          )
          (def i (+ i 1))
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
(assert (= (removeNthFromEnd [1 2 3 4 5] 2) [1 2 3 5]))
)

(defn test_example_2 []
(assert (= (removeNthFromEnd [1] 1) []))
)

(defn test_example_3 []
(assert (= (removeNthFromEnd [1 2] 1) [1]))
)

(defn test_remove_first []
(assert (= (removeNthFromEnd [7 8 9] 3) [8 9]))
)

(defn test_remove_last []
(assert (= (removeNthFromEnd [7 8 9] 1) [7 8]))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
(test_remove_first)
(test_remove_last)
)

(-main)
