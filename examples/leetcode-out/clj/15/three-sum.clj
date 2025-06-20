(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn threeSum [nums]
  (try
    (def sorted (vec (map (fn [x] x) (sort-by (fn [x] x) nums))))
    (def n (count sorted))
    (def res [])
    (def i 0)
    (loop []
      (when (< i n)
        (let [r (try
          (when (and (> i 0) (= (_indexList sorted i) (_indexList sorted (- i 1))))
            (def i (+ i 1))
            (throw (ex-info "continue" {}))
          )
          (def left (+ i 1))
          (def right (- n 1))
          (loop []
            (when (< left right)
              (let [r (try
                (def sum (+ (+ (_indexList sorted i) (_indexList sorted left)) (_indexList sorted right)))
                (if (= sum 0)
                  (do
                    (def res (vec (concat res [[(_indexList sorted i) (_indexList sorted left) (_indexList sorted right)]])))
                    (def left (+ left 1))
                    (loop []
                      (when (and (< left right) (= (_indexList sorted left) (_indexList sorted (- left 1))))
                        (let [r (try
                          (def left (+ left 1))
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
                (def right (- right 1))
                (loop []
                  (when (and (< left right) (= (_indexList sorted right) (_indexList sorted (+ right 1))))
                    (let [r (try
                      (def right (- right 1))
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
          )
        
        (if (< sum 0)
          (do
            (def left (+ left 1))
          )
        
        (do
          (def right (- right 1))
        )
        )
        )
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
(throw (ex-info "return" {:value res}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (threeSum [(- 1) 0 1 2 (- 1) (- 4)]) [[(- 1) (- 1) 2] [(- 1) 0 1]]))
)

(defn test_example_2 []
(assert (= (threeSum [0 1 1]) []))
)

(defn test_example_3 []
(assert (= (threeSum [0 0 0]) [[0 0 0]]))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
)

(-main)
