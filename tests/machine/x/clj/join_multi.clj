(ns main)

(declare customers orders items result)

(defn -main []
  (def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"}]) ;; list of 
  (def orders [{:id 100 :customerId 1} {:id 101 :customerId 2}]) ;; list of 
  (def items [{:orderId 100 :sku "a"} {:orderId 101 :sku "b"}]) ;; list of 
  (def result (vec (->> (for [o orders c customers :when (= (:customerId o) (:id c)) i items :when (= (:id o) (:orderId i))] {:name (:name c) :sku (:sku i)})))) ;; list of map of string to string
  (println "--- Multi Join ---")
  (loop [_tmp0 (seq result)]
    (when _tmp0
      (let [r (clojure.core/first _tmp0)]
        (let [r (try
          (println (:name r) "bought item" (:sku r))
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
