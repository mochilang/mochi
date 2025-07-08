(ns main)

(declare i)

(defn -main []
  (def i 0) ;; int
  (loop []
    (when (< i 3)
      (let [r (try
        (println i)
        (def i (+ i 1)) ;; int
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
)

(-main)
