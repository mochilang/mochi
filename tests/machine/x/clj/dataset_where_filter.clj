(ns main)

(declare people adults)

(defn -main []
  (def people [{:name "Alice" :age 30} {:name "Bob" :age 15} {:name "Charlie" :age 65} {:name "Diana" :age 45}]) ;; list of map of string to any
  (def adults (vec (->> (for [person people :when (>= (:age person) 18)] {:name (:name person) :age (:age person) :is_senior (>= (:age person) 60)})))) ;; list of map of string to any
  (println "--- Adults ---")
  (loop [_tmp0 (seq adults)]
    (when _tmp0
      (let [person (clojure.core/first _tmp0)]
        (let [r (try
          (println (:name person) "is" (:age person) (if (:is_senior person) " (senior)" ""))
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
