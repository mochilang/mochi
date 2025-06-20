(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn fourSum [nums target]
  (try
    (def sorted (vec (map (fn [n] n) (sort-by (fn [n] n) nums))))
    (def n (count sorted))
    (def result [])
    (loop [i 0]
      (when (< i n)
        (let [r (try
          (when (and (> i 0) (= (_indexList sorted i) (_indexList sorted (- i 1))))
            (throw (ex-info "continue" {}))
          )
          (loop [j (+ i 1)]
            (when (< j n)
              (let [r (try
                (when (and (> j (+ i 1)) (= (_indexList sorted j) (_indexList sorted (- j 1))))
                  (throw (ex-info "continue" {}))
                )
                (def left (+ j 1))
                (def right (- n 1))
                (loop []
                  (when (< left right)
                    (let [r (try
                      (def sum (+ (+ (+ (_indexList sorted i) (_indexList sorted j)) (_indexList sorted left)) (_indexList sorted right)))
                      (if (= sum target)
                        (do
                          (def result (vec (concat result [[(_indexList sorted i) (_indexList sorted j) (_indexList sorted left) (_indexList sorted right)]])))
                          (def left (+ left 1))
                          (def right (- right 1))
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
              
              (if (< sum target)
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
  :else (recur (inc j))
)
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
:else (recur (inc i))
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
(assert (= (fourSum [1 0 (- 1) 0 (- 2) 2] 0) [[(- 2) (- 1) 1 2] [(- 2) 0 0 2] [(- 1) 0 0 1]]))
)

(defn test_example_2 []
(assert (= (fourSum [2 2 2 2 2] 8) [[2 2 2 2]]))
)

(defn -main []
(test_example_1)
(test_example_2)
)

(-main)
