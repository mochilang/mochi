(ns main)

(declare nums letters pairs)

(defn -main []
  (def nums [1 2 3]) ;; list of int
  (def letters ["A" "B"]) ;; list of string
  (def pairs (vec (->> (for [n nums :when (= (mod n 2) 0) l letters] {:n n :l l})))) ;; list of map of string to any
  (println "--- Even pairs ---")
  (loop [_tmp0 (seq pairs)]
    (when _tmp0
      (let [p (clojure.core/first _tmp0)]
        (let [r (try
          (println (:n p) (:l p))
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
