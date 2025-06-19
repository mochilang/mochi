(defn convert [s numRows]
  (try
    (when (or (<= numRows 1) (>= numRows (count s)))
      (throw (ex-info "return" {:value s}))
    )
    (def rows [])
    (def i 0)
    (loop []
      (when (< i numRows)
        (let [r (try
          (def rows (vec (concat rows [""])))
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
(def curr 0)
(def step 1)
(loop [_tmp0 (seq s)]
  (when _tmp0
    (let [ch (first _tmp0)]
      (let [r (try
        (def rows (assoc rows curr (str (nth rows curr) ch)))
        (if (= curr 0)
          (do
            (def step 1)
          )
        
        (when (= curr (- numRows 1))
          (def step (- 1))
        )
        )
        (def curr (+ curr step))
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
      :else (recur (next _tmp0))
    )
  )
)
)
)
(def result "")
(loop [_tmp1 (seq rows)]
(when _tmp1
(let [row (first _tmp1)]
  (let [r (try
    (def result (str result row))
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
  :else (recur (next _tmp1))
)
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
(assert (= (convert "PAYPALISHIRING" 3) "PAHNAPLSIIGYIR"))
)

(defn test_example_2 []
(assert (= (convert "PAYPALISHIRING" 4) "PINALSIGYAHRPI"))
)

(defn test_single_row []
(assert (= (convert "A" 1) "A"))
)

(test_example_1)
(test_example_2)
(test_single_row)
