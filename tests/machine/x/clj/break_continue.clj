(ns main)

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

(declare numbers)

(defn -main []
  (def numbers [1 2 3 4 5 6 7 8 9]) ;; list of int
  (loop [_tmp0 (seq numbers)]
    (when _tmp0
      (let [n (clojure.core/first _tmp0)]
        (let [r (try
          (when (_equal (mod n 2) 0)
            (throw (ex-info "continue" {}))
          )
          (when (> n 7)
            (throw (ex-info "break" {}))
          )
          (println "odd number:" n)
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
)

(-main)
