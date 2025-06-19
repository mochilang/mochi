(defn digit [ch]
  (try
    (when (= ch "0")
      (throw (ex-info "return" {:value 0}))
    )
    (when (= ch "1")
      (throw (ex-info "return" {:value 1}))
    )
    (when (= ch "2")
      (throw (ex-info "return" {:value 2}))
    )
    (when (= ch "3")
      (throw (ex-info "return" {:value 3}))
    )
    (when (= ch "4")
      (throw (ex-info "return" {:value 4}))
    )
    (when (= ch "5")
      (throw (ex-info "return" {:value 5}))
    )
    (when (= ch "6")
      (throw (ex-info "return" {:value 6}))
    )
    (when (= ch "7")
      (throw (ex-info "return" {:value 7}))
    )
    (when (= ch "8")
      (throw (ex-info "return" {:value 8}))
    )
    (when (= ch "9")
      (throw (ex-info "return" {:value 9}))
    )
    (throw (ex-info "return" {:value (- 1)}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(defn myAtoi [s]
  (try
    (def i 0)
    (def n (count s))
    (loop []
      (when (and (< i n) (= (nth s i) (nth " " 0)))
        (let [r (try
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
(def sign 1)
(when (and (< i n) (or (= (nth s i) (nth "+" 0)) (= (nth s i) (nth "-" 0))))
  (when (= (nth s i) (nth "-" 0))
    (def sign (- 1))
  )
  (def i (+ i 1))
)
(def result 0)
(loop []
  (when (< i n)
    (let [r (try
      (def ch (subs s i (+ i 1)))
      (def d (digit ch))
      (when (< d 0)
        (throw (ex-info "break" {}))
      )
      (def result (+ (* result 10) d))
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
(def result (* result sign))
(when (> result 2147483647)
(throw (ex-info "return" {:value 2147483647}))
)
(when (< result (- 2147483648))
(throw (ex-info "return" {:value (- 2147483648)}))
)
(throw (ex-info "return" {:value result}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (myAtoi "42") 42))
)

(defn test_example_2 []
(assert (= (myAtoi "   -42") (- 42)))
)

(defn test_example_3 []
(assert (= (myAtoi "4193 with words") 4193))
)

(defn test_example_4 []
(assert (= (myAtoi "words and 987") 0))
)

(defn test_example_5 []
(assert (= (myAtoi "-91283472332") (- 2147483648)))
)

(test_example_1)
(test_example_2)
(test_example_3)
(test_example_4)
(test_example_5)
