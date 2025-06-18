(defn expand [s left right]
  (try
    (def l left)
    (def r right)
    (def n (count s))
    (try
      (loop []
        (when (and (>= l 0) (< r n))
          (when (not= (nth s l) (nth s r))
            (throw (ex-info "break" {}))
          )
          (def l (- l 1))
          (def r (+ r 1))
          (recur)
        )
      )
    (catch clojure.lang.ExceptionInfo e
      (when-not (= (.getMessage e) "break") (throw e))
    )
  )
  (throw (ex-info "return" {:value (- (- r l) 1)}))
(catch clojure.lang.ExceptionInfo e
  (if (= (.getMessage e) "return")
    (:value (ex-data e))
  (throw e)))
)
)

(defn longestPalindrome [s]
(try
  (when (<= (count s) 1)
    (throw (ex-info "return" {:value s}))
  )
  (def start 0)
  (def end 0)
  (def n (count s))
  (loop [i 0]
    (when (< i n)
      (def len1 (expand s i i))
      (def len2 (expand s i (+ i 1)))
      (def l len1)
      (when (> len2 len1)
        (def l len2)
      )
      (when (> l (- end start))
        (def start (- i (quot (- l 1) 2)))
        (def end (+ i (quot l 2)))
      )
      (recur (inc i)))
  )
  (throw (ex-info "return" {:value (subs s start (+ end 1))}))
(catch clojure.lang.ExceptionInfo e
  (if (= (.getMessage e) "return")
    (:value (ex-data e))
  (throw e)))
)
)

(defn test_example_1 []
(def ans (longestPalindrome "babad"))
(assert (or (= ans "bab") (= ans "aba")))
)

(defn test_example_2 []
(assert (= (longestPalindrome "cbbd") "bb"))
)

(defn test_single_char []
(assert (= (longestPalindrome "a") "a"))
)

(defn test_two_chars []
(def ans (longestPalindrome "ac"))
(assert (or (= ans "a") (= ans "c")))
)

(test_example_1)
(test_example_2)
(test_single_char)
(test_two_chars)
