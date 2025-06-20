(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn maxArea [height]
  (try
    (def left 0)
    (def right (- (count height) 1))
    (def maxArea 0)
    (loop []
      (when (< left right)
        (let [r (try
          (def width (- right left))
          (def h 0)
          (if (< (_indexList height left) (_indexList height right))
            (do
              (def h (_indexList height left))
            )
          
          (do
            (def h (_indexList height right))
          )
          )
          (def area (* h width))
          (when (> area maxArea)
            (def maxArea area)
          )
          (if (< (_indexList height left) (_indexList height right))
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
(throw (ex-info "return" {:value maxArea}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (maxArea [1 8 6 2 5 4 8 3 7]) 49))
)

(defn test_example_2 []
(assert (= (maxArea [1 1]) 1))
)

(defn test_decreasing_heights []
(assert (= (maxArea [4 3 2 1 4]) 16))
)

(defn test_short_array []
(assert (= (maxArea [1 2 1]) 2))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_decreasing_heights)
(test_short_array)
)

(-main)
