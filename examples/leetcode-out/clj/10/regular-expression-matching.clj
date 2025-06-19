(defn isMatch [s p]
  (try
    (def m (count s))
    (def n (count p))
    (def dp [])
    (def i 0)
    (loop []
      (when (<= i m)
        (let [r (try
          (def row [])
          (def j 0)
          (loop []
            (when (<= j n)
              (let [r (try
                (def row (vec (concat row [false])))
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
      (def dp (vec (concat dp [row])))
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
(def dp (assoc-in dp [m n] true))
(def i2 m)
(loop []
(when (>= i2 0)
(let [r (try
  (def j2 (- n 1))
  (loop []
    (when (>= j2 0)
      (let [r (try
        (def first false)
        (when (< i2 m)
          (when (or (= (nth p j2) (nth s i2)) (= (nth p j2) "."))
            (def first true)
          )
        )
        (def star false)
        (when (< (+ j2 1) n)
          (when (= (nth p (+ j2 1)) "*")
            (def star true)
          )
        )
        (if star
          (do
            (if (or (nth (nth dp i2) (+ j2 2)) (and first (nth (nth dp (+ i2 1)) j2)))
              (do
                (def dp (assoc-in dp [i2 j2] true))
              )
            
            (do
              (def dp (assoc-in dp [i2 j2] false))
            )
            )
          )
        
        (do
          (if (and first (nth (nth dp (+ i2 1)) (+ j2 1)))
            (do
              (def dp (assoc-in dp [i2 j2] true))
            )
          
          (do
            (def dp (assoc-in dp [i2 j2] false))
          )
          )
        )
        )
        (def j2 (- j2 1))
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
(def i2 (- i2 1))
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
(throw (ex-info "return" {:value (nth (nth dp 0) 0)}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (isMatch "aa" "a") false))
)

(defn test_example_2 []
(assert (= (isMatch "aa" "a*") true))
)

(defn test_example_3 []
(assert (= (isMatch "ab" ".*") true))
)

(defn test_example_4 []
(assert (= (isMatch "aab" "c*a*b") true))
)

(defn test_example_5 []
(assert (= (isMatch "mississippi" "mis*is*p*.") false))
)

(test_example_1)
(test_example_2)
(test_example_3)
(test_example_4)
(test_example_5)
