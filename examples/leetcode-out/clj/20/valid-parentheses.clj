(ns main)

(defn _indexString [s i]
  (let [r (vec (seq s))
        i (if (neg? i) (+ i (count r)) i)]
    (if (or (< i 0) (>= i (count r)))
      (throw (ex-info "index out of range" {}))
      (str (nth r i)))))

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn isValid [s]
  (try
    (def stack [])
    (def n (count s))
    (loop [i 0]
      (when (< i n)
        (let [r (try
          (def c (_indexString s i))
          (if (= c "(")
            (do
              (def stack (vec (concat stack [")"])))
            )
          
          (if (= c "[")
            (do
              (def stack (vec (concat stack ["]"])))
            )
          
          (if (= c "{")
            (do
              (def stack (vec (concat stack ["}"])))
            )
          
          (do
            (when (= (count stack) 0)
              (throw (ex-info "return" {:value false}))
            )
            (def top (_indexList stack (- (count stack) 1)))
            (when (not= top c)
              (throw (ex-info "return" {:value false}))
            )
            (def stack (subvec stack 0 (- (count stack) 1)))
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
(throw (ex-info "return" {:value (= (count stack) 0)}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (isValid "()") true))
)

(defn test_example_2 []
(assert (= (isValid "()[]{}") true))
)

(defn test_example_3 []
(assert (= (isValid "(]") false))
)

(defn test_example_4 []
(assert (= (isValid "([)]") false))
)

(defn test_example_5 []
(assert (= (isValid "{[]}") true))
)

(defn test_empty_string []
(assert (= (isValid "") true))
)

(defn test_single_closing []
(assert (= (isValid "]") false))
)

(defn test_unmatched_open []
(assert (= (isValid "((") false))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
(test_example_4)
(test_example_5)
(test_empty_string)
(test_single_closing)
(test_unmatched_open)
)

(-main)
