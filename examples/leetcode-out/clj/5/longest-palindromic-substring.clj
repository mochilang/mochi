(ns main)

(defn _indexString [s i]
  (let [r (vec (seq s))
        i (if (neg? i) (+ i (count r)) i)]
    (if (or (< i 0) (>= i (count r)))
      (throw (ex-info "index out of range" {}))
      (str (nth r i)))))

(defn expand [s left right]
  (try
    (def l left)
    (def r right)
    (def n (count s))
    (loop []
      (when (and (>= l 0) (< r n))
        (let [r (try
          (when (not= (_indexString s l) (_indexString s r))
            (throw (ex-info "break" {}))
          )
          (def l (- l 1))
          (def r (+ r 1))
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
    (let [r (try
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
(throw (ex-info "return" {:value (subs s start (+ end 1))}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(def ans (longestPalindrome "babad"))
(assert (or (= ans "bab") (= ans "aba")) "expect failed")
)

(defn test_example_2 []
(assert (= (longestPalindrome "cbbd") "bb") "expect failed")
)

(defn test_single_char []
(assert (= (longestPalindrome "a") "a") "expect failed")
)

(defn test_two_chars []
(def ans (longestPalindrome "ac"))
(assert (or (= ans "a") (= ans "c")) "expect failed")
)

(defn -main []
(test_example_1)
(test_example_2)
(test_single_char)
(test_two_chars)
)

(-main)
