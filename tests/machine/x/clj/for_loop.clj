(ns main)

(defn -main []
  (loop [i 1]
    (when (< i 4)
      (let [r (try
        (println i)
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
)

(-main)
