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

(declare customers orders result)

(defn -main []
  (def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"} {:id 3 :name "Charlie"}]) ;; list of 
  (def orders [{:id 100 :customerId 1 :total 250} {:id 101 :customerId 2 :total 125} {:id 102 :customerId 1 :total 300} {:id 103 :customerId 4 :total 80}]) ;; list of 
  (def result (vec (->> (for [o orders c customers :when (_equal (:customerId o) (:id c))] {:orderId (:id o) :customerName (:name c) :total (:total o)})))) ;; list of 
  (println "--- Orders with customer info ---")
  (loop [_tmp0 (seq result)]
    (when _tmp0
      (let [entry (clojure.core/first _tmp0)]
        (let [r (try
          (println "Order" (:orderId entry) "by" (:customerName entry) "- $" (:total entry))
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
