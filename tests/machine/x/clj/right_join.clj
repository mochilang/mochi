(ns main)

(defn _query [src joins opts]
  (let [items (atom (mapv vector src))]
    (doseq [j joins]
      (let [joined (atom [])]
        (cond
          (and (:right j) (:left j))
            (let [matched (boolean-array (count (:items j)))]
              (doseq [left @items]
                (let [m (atom false)]
                  (doseq [[ri right] (map-indexed vector (:items j))]
                    (let [keep (if-let [f (:on j)]
                                 (apply f (conj left right))
                                 true)]
                      (when keep
                        (reset! m true)
                        (aset matched ri true)
                        (swap! joined conj (conj left right))))
                  (when-not @m
                    (swap! joined conj (conj left nil))))
              (doseq [[ri right] (map-indexed vector (:items j))]
                (when-not (aget matched ri)
                  (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
            (reset! items @joined)
          (:right j)
            (do
              (doseq [right (:items j)]
                (let [m (atom false)]
                  (doseq [left @items]
                    (let [keep (if-let [f (:on j)]
                                 (apply f (conj left right))
                                 true)]
                      (when keep
                        (reset! m true)
                        (swap! joined conj (conj left right))))
                  (when-not @m
                    (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
              (reset! items @joined))
          :else
            (do
              (doseq [left @items]
                (let [m (atom false)]
                  (doseq [right (:items j)]
                    (let [keep (if-let [f (:on j)]
                                 (apply f (conj left right))
                                 true)]
                      (when keep
                        (reset! m true)
                        (swap! joined conj (conj left right))))
                  (when (and (:left j) (not @m))
                    (swap! joined conj (conj left nil))))
              (reset! items @joined))))
    (let [it @items
          it (if-let [w (:where opts)] (vec (filter #(apply w %) it)) it)
          it (if-let [sk (:sortKey opts)] (vec (sort-by #(apply sk %) it)) it)
          it (if (contains? opts :skip) (vec (drop (:skip opts) it)) it)
          it (if (contains? opts :take) (vec (take (:take opts) it)) it)]
      (mapv #(apply (:select opts) %) it))))))))))))
(declare customers orders result)

(defn -main []
  (def customers [{:id 1 :name "Alice"} {:id 2 :name "Bob"} {:id 3 :name "Charlie"} {:id 4 :name "Diana"}]) ;; list of map of string to any
  (def orders [{:id 100 :customerId 1 :total 250} {:id 101 :customerId 2 :total 125} {:id 102 :customerId 1 :total 300}]) ;; list of map of string to int
  (def result (let [_src customers]
  (_query _src [
    {:items orders :on (fn [c o] (= (:customerId o) (:id c))) :right true}
  ] { :select (fn [c o] {:customerName (:name c) :order o}) }))) ;; list of map of string to any
  (println "--- Right Join using syntax ---")
  (loop [_tmp0 (seq result)]
    (when _tmp0
      (let [entry (clojure.core/first _tmp0)]
        (let [r (try
          (if (:order entry)
            (do
              (println "Customer" (:customerName entry) "has order" (:id (:order entry)) "- $" (:total (:order entry)))
            )
          
          (do
            (println "Customer" (:customerName entry) "has no orders")
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
