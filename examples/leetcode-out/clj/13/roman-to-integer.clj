(ns main)

(defn _indexString [s i]
  (let [r (vec (seq s))
        i (if (neg? i) (+ i (count r)) i)]
    (if (or (< i 0) (>= i (count r)))
      (throw (ex-info "index out of range" {}))
      (str (nth r i)))))

(defn romanToInt [s]
  (try
    (def values {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000})
    (def total 0)
    (def i 0)
    (def n (count s))
    (loop []
      (when (< i n)
        (let [r (try
          (def curr (get values (_indexString s i)))
          (when (< (+ i 1) n)
            (def next (get values (_indexString s (+ i 1))))
            (when (< curr next)
              (def total (- (+ total next) curr))
              (def i (+ i 2))
              (throw (ex-info "continue" {}))
            )
          )
          (def total (+ total curr))
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
(throw (ex-info "return" {:value total}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (romanToInt "III") 3))
)

(defn test_example_2 []
(assert (= (romanToInt "LVIII") 58))
)

(defn test_example_3 []
(assert (= (romanToInt "MCMXCIV") 1994))
)

(defn test_subtractive []
(assert (= (romanToInt "IV") 4))
(assert (= (romanToInt "IX") 9))
)

(defn test_tens []
(assert (= (romanToInt "XL") 40))
(assert (= (romanToInt "XC") 90))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
(test_subtractive)
(test_tens)
)

(-main)
