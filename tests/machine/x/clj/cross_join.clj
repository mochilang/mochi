(ns main)

(declare customers orders result)

(defn -main []
  (def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"} {:id 3 :name "Charlie"}]) ;; list of 
  (def orders [{:id 100 :customerId 1 :total 250} {:id 101 :customerId 2 :total 125} {:id 102 :customerId 1 :total 300}]) ;; list of 
  (def result (vec (->> (for [o orders c customers] {:orderId (:id o) :orderCustomerId (:customerId o) :pairedCustomerName (:name c) :orderTotal (:total o)})))) ;; list of map of string to any
  (println "--- Cross Join: All order-customer pairs ---")
  (loop [_tmp0 (seq result)]
    (when _tmp0
      (let [entry (clojure.core/first _tmp0)]
        (let [r (try
          (println "Order" (:orderId entry) "(customerId:" (:orderCustomerId entry) ", total: $" (:orderTotal entry) ") paired with" (:pairedCustomerName entry))
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
