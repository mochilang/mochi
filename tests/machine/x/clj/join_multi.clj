; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:02:34Z
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
(declare customers orders items result)

(defn -main []
  (def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"}]) ;; list of
  (def orders [{:id 100 :customerId 1} {:id 101 :customerId 2}]) ;; list of
  (def items [{:orderId 100 :sku "a"} {:orderId 101 :sku "b"}]) ;; list of
  (def result (vec (->> (for [o orders c customers :when (= (:customerId o) (:id c)) i items :when (= (:id o) (:orderId i))] {:name (:name c) :sku (:sku i)})))) ;; list of map of string to string
  (_print "--- Multi Join ---")
  (loop [_tmp0 (seq result)]
    (when _tmp0
      (let [r (clojure.core/first _tmp0)]
        (let [r (try
          (_print (:name r) "bought item" (:sku r))
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
