(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn intToRoman [num]
  (try
    (def values [1000 900 500 400 100 90 50 40 10 9 5 4 1])
    (def symbols ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"])
    (def result "")
    (def i 0)
    (loop []
      (when (> num 0)
        (let [r (try
          (loop []
            (when (>= num (_indexList values i))
              (let [r (try
                (def result (str result (_indexList symbols i)))
                (def num (- num (_indexList values i)))
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
(throw (ex-info "return" {:value result}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (intToRoman 3) "III"))
)

(defn test_example_2 []
(assert (= (intToRoman 58) "LVIII"))
)

(defn test_example_3 []
(assert (= (intToRoman 1994) "MCMXCIV"))
)

(defn test_small_numbers []
(assert (= (intToRoman 4) "IV"))
(assert (= (intToRoman 9) "IX"))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
(test_small_numbers)
)

(-main)
