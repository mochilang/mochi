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
          (when (or (= (_indexString p j2) (_indexString s i2)) (= (_indexString p j2) "."))
            (def first true)
          )
        )
        (def star false)
        (when (< (+ j2 1) n)
          (when (= (_indexString p (+ j2 1)) "*")
            (def star true)
          )
        )
        (if star
          (do
            (def ok false)
            (if (_indexList (_indexList dp i2) (+ j2 2))
              (do
                (def ok true)
              )
            
            (do
              (when first
                (when (_indexList (_indexList dp (+ i2 1)) j2)
                  (def ok true)
                )
              )
            )
            )
            (def dp (assoc-in dp [i2 j2] ok))
          )
        
        (do
          (def ok false)
          (when first
            (when (_indexList (_indexList dp (+ i2 1)) (+ j2 1))
              (def ok true)
            )
          )
          (def dp (assoc-in dp [i2 j2] ok))
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
(throw (ex-info "return" {:value (_indexList (_indexList dp 0) 0)}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (isMatch "aa" "a") false) "expect failed")
)

(defn test_example_2 []
(assert (= (isMatch "aa" "a*") true) "expect failed")
)

(defn test_example_3 []
(assert (= (isMatch "ab" ".*") true) "expect failed")
)

(defn test_example_4 []
(assert (= (isMatch "aab" "c*a*b") true) "expect failed")
)

(defn test_example_5 []
(assert (= (isMatch "mississippi" "mis*is*p*.") false) "expect failed")
)

(defn -main []
(test_example_1)
(test_example_2)
(test_example_3)
(test_example_4)
(test_example_5)
)

(-main)
