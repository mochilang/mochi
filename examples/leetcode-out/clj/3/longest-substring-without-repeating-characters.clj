(defn lengthOfLongestSubstring [s]
  (try
    (def n (count s))
    (def start 0)
    (def best 0)
    (def i 0)
    (loop []
      (when (< i n)
        (let [r (try
          (def j start)
          (loop []
            (when (< j i)
              (let [r (try
                (when (= (nth s j) (nth s i))
                  (def start (+ j 1))
                  (throw (ex-info "break" {}))
                )
                (def j (+ j 1))
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
      (def length (+ (- i start) 1))
      (when (> length best)
        (def best length)
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
(throw (ex-info "return" {:value best}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (lengthOfLongestSubstring "abcabcbb") 3))
)

(defn test_example_2 []
(assert (= (lengthOfLongestSubstring "bbbbb") 1))
)

(defn test_example_3 []
(assert (= (lengthOfLongestSubstring "pwwkew") 3))
)

(defn test_empty_string []
(assert (= (lengthOfLongestSubstring "") 0))
)

(test_example_1)
(test_example_2)
(test_example_3)
(test_empty_string)
