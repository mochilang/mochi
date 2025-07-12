(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(defn _equal [a b]
  (cond
    (and (sequential? a) (sequential? b))
      (and (= (count a) (count b)) (every? true? (map _equal a b)))
    (and (map? a) (map? b))
      (and (= (count a) (count b))
           (every? (fn [k] (_equal (get a k) (get b k))) (keys a)))
    (and (number? a) (number? b))
      (= (double a) (double b))
    :else
      (= a b)))

(declare result)

;; Function twoSum takes [nums: list of int, target: int] and returns list of int
(defn twoSum [nums target]
  (try
    (def n (count nums)) ;; int
    (loop [i 0]
      (when (< i n)
        (let [r (try
          (loop [j (+ i 1)]
            (when (< j n)
              (let [r (try
                (when (_equal (+ (_indexList nums i) (_indexList nums j)) target)
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

(defn -main []
(def result (twoSum [2 7 11 15] 9)) ;; list of int
(println (_indexList result 0))
(println (_indexList result 1))
)

(-main)
