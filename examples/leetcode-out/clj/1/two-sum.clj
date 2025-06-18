(defn twoSum [nums target]
  (try
    (def n (count nums))
    (loop [i 0]
      (when (< i n)
        (loop [j (+ i 1)]
          (when (< j n)
            (when (= (+ (nth nums i) (nth nums j)) target)
              (throw (ex-info "return" {:value [i j]}))
            )
            (recur (inc j)))
        )
        (recur (inc i)))
    )
    (throw (ex-info "return" {:value [(- 1) (- 1)]}))
  (catch clojure.lang.ExceptionInfo e
    (if (= (.getMessage e) "return")
      (:value (ex-data e))
    (throw e)))
  )
)

(def result (twoSum [2 7 11 15] 9))
(println (nth result 0))
(println (nth result 1))
