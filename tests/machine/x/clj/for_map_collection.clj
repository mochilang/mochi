(ns main)

(declare m)

(defn -main []
  (def m {"a" 1 "b" 2}) ;; map of string to int
  (loop [_tmp0 (seq m)]
    (when _tmp0
      (let [k (clojure.core/first _tmp0)]
        (let [r (try
          (println k)
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
