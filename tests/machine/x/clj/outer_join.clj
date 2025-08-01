; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:03:09Z
(ns main)

(defn _print [& args]
  (letfn [(pv [v]
            (cond
              (true? v) (print 1)
              (false? v) (print 0)
              (sequential? v) (doseq [[i x] (map-indexed vector v)]
                                (when (> i 0) (print " "))
                                (pv x))
              :else (print v)))]
    (doseq [[i a] (map-indexed vector args)]
      (when (> i 0) (print " "))
      (pv a))
    (println)))
(declare customers orders result)

(defn -main []
  (def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"} {:id 3 :name "Charlie"} {:id 4 :name "Diana"}]) ;; list of
  (def orders [{:id 100 :customerId 1 :total 250} {:id 101 :customerId 2 :total 125} {:id 102 :customerId 1 :total 300} {:id 103 :customerId 5 :total 80}]) ;; list of
  (def result (vec (concat (for [o orders] (let [c (some (fn [c] (when (= (:customerId o) (:id c)) c)) customers)] {:order o :customer c})) (for [c customers :when (not-any? (fn [o] (= (:customerId o) (:id c))) orders)] (let [o nil] {:order o :customer c}))))) ;; list of map of string to any
  (_print "--- Outer Join using syntax ---")
  (loop [_tmp0 (seq result)]
    (when _tmp0
      (let [row (clojure.core/first _tmp0)]
        (let [r (try
          (if (:order row)
            (do
              (if (:customer row)
                (do
                  (_print "Order" (:id (:order row)) "by" (:name (:customer row)) "- $" (:total (:order row)))
                )

              (do
                (_print "Order" (:id (:order row)) "by" "Unknown" "- $" (:total (:order row)))
              )
              )
            )

          (do
            (_print "Customer" (:name (:customer row)) "has no orders")
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
        :else (recur (next _tmp0))
      )
    )
  )
)
)
)

(-main)
