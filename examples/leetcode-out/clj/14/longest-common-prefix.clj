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

(defn longestCommonPrefix [strs]
  (try
    (when (= (count strs) 0)
      (throw (ex-info "return" {:value ""}))
    )
    (def prefix (_indexList strs 0))
    (loop [i 1]
      (when (< i (count strs))
        (let [r (try
          (def j 0)
          (def current (_indexList strs i))
          (loop []
            (when (and (< j (count prefix)) (< j (count current)))
              (let [r (try
                (when (not= (_indexString prefix j) (_indexString current j))
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
      (def prefix (subs prefix 0 j))
      (when (= prefix "")
        (throw (ex-info "break" {}))
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
(throw (ex-info "return" {:value prefix}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
(:value (ex-data e))
(throw e)))
)
)

(defn test_example_1 []
(assert (= (longestCommonPrefix ["flower" "flow" "flight"]) "fl"))
)

(defn test_example_2 []
(assert (= (longestCommonPrefix ["dog" "racecar" "car"]) ""))
)

(defn test_single_string []
(assert (= (longestCommonPrefix ["single"]) "single"))
)

(defn test_no_common_prefix []
(assert (= (longestCommonPrefix ["a" "b" "c"]) ""))
)

(defn -main []
(test_example_1)
(test_example_2)
(test_single_string)
(test_no_common_prefix)
)

(-main)
