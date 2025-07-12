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

(declare nums letters pairs)

(defn -main []
  (def nums [1 2 3]) ;; list of int
  (def letters ["A" "B"]) ;; list of string
  (def pairs (vec (->> (for [n nums :when (_equal (mod n 2) 0) l letters] {:n n :l l})))) ;; list of 
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
