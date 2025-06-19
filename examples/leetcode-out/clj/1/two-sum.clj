(defn twoSum [nums target]
  (try
    (def n (count nums))
    (loop [i 0]
      (when (< i n)
        (let [r (try
          (loop [j (+ i 1)]
            (when (< j n)
              (let [r (try
                (when (= (+ (nth nums i) (nth nums j)) target)
                  (throw (ex-info "return" {:value [i j]}))
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
              :else (recur (inc j))
            )
          )
        )
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
