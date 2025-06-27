(ns main)

(defn _indexString [s i]
  (let [r (vec (seq s))
        i (if (neg? i) (+ i (count r)) i)]
    (if (or (< i 0) (>= i (count r)))
      (throw (ex-info "index out of range" {}))
      (str (nth r i)))))

(defn isPalindrome [x]
  (try
    (when (< x 0)
      (throw (ex-info "return" {:value false}))
    )
    (def s (str x))
    (def n (count s))
    (loop [i 0]
      (when (< i (quot n 2))
        (let [r (try
          (when (not= (_indexString s i) (_indexString s (- (- n 1) i)))
            (throw (ex-info "return" {:value false}))
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
(throw (ex-info "return" {:value true}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (isPalindrome 121) true) "expect failed")
)

(defn test_example_2 []
(assert (= (isPalindrome (- 121)) false) "expect failed")
)

(defn test_example_3 []
(assert (= (isPalindrome 10) false) "expect failed")
)

(defn test_zero []
(assert (= (isPalindrome 0) true) "expect failed")
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
(test_zero)
)

(-main)
