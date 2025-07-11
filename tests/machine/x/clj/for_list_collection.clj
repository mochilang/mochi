(ns main)

(defn -main []
  (loop [_tmp0 (seq [1 2 3])]
    (when _tmp0
      (let [n (clojure.core/first _tmp0)]
        (let [r (try
          (println n)
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
