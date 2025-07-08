(ns main)

(declare nums letters bools combos)

(defn -main []
  (def nums [1 2]) ;; list of int
  (def letters ["A" "B"]) ;; list of string
  (def bools [true false]) ;; list of bool
  (def combos (vec (->> (for [n nums l letters b bools] {:n n :l l :b b})))) ;; list of map of string to any
  (println "--- Cross Join of three lists ---")
  (loop [_tmp0 (seq combos)]
    (when _tmp0
      (let [c (clojure.core/first _tmp0)]
        (let [r (try
          (println (:n c) (:l c) (:b c))
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
