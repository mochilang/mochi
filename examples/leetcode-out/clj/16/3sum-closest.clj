(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn threeSumClosest [nums target]
  (try
    (def sorted (vec (map (fn [n] n) (sort-by (fn [n] n) nums))))
    (def n (count sorted))
    (def best (+ (+ (_indexList sorted 0) (_indexList sorted 1)) (_indexList sorted 2)))
    (loop [i 0]
      (when (< i n)
        (let [r (try
          (def left (+ i 1))
          (def right (- n 1))
          (loop []
            (when (< left right)
              (let [r (try
                (def sum (+ (+ (_indexList sorted i) (_indexList sorted left)) (_indexList sorted right)))
                (when (= sum target)
                  (throw (ex-info "return" {:value target}))
                )
                (def diff 0)
                (if (> sum target)
                  (do
                    (def diff (- sum target))
                  )
                
                (do
                  (def diff (- target sum))
                )
                )
                (def bestDiff 0)
                (if (> best target)
                  (do
                    (def bestDiff (- best target))
                  )
                
                (do
                  (def bestDiff (- target best))
                )
                )
                (when (< diff bestDiff)
                  (def best sum)
                )
                (if (< sum target)
                  (do
                    (def left (+ left 1))
                  )
                
                (do
                  (def right (- right 1))
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
    :else (recur (inc i))
  )
)
)
)
(throw (ex-info "return" {:value best}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (threeSumClosest [(- 1) 2 1 (- 4)] 1) 2))
)

(defn test_example_2 []
(assert (= (threeSumClosest [0 0 0] 1) 0))
)

(defn test_additional []
(assert (= (threeSumClosest [1 1 1 0] (- 100)) 2))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_additional)
)

(-main)
