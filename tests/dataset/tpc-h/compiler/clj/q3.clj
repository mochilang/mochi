(ns main)

(defn _sum [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "sum() expects list or group" {})))]
    (reduce + 0 lst))
  )

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

(defrecord _Group [key Items])

(defn _group_by [src keyfn]
  (let [groups (transient {})
        order (transient [])]
    (doseq [it src]
      (let [k (keyfn it)
            ks (str k)
            g (get groups ks)]
        (if g
          (assoc! groups ks (assoc g :Items (conj (:Items g) it)))
          (do
            (assoc! groups ks (_Group. k [it]))
            (conj! order ks))))
    )
    (let [g (persistent! groups)
          o (persistent! order)]
      (mapv #(get g %) o))))

(defn _escape_json [s]
  (-> s
      (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")))

(defn _to_json [v]
  (cond
    (nil? v) "null"
    (string? v) (str "\"" (_escape_json v) "\"")
    (number? v) (str v)
    (boolean? v) (str v)
    (sequential? v) (str "[" (clojure.string/join "," (map _to_json v)) "]")
    (map? v) (str "{" (clojure.string/join "," (map (fn [[k val]]
                                        (str "\"" (_escape_json (name k)) "\":" (_to_json val))) v)) "}")
    :else (str "\"" (_escape_json (str v)) "\"")))

(defn _json [v]
  (println (_to_json v)))

(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))
(defn _query [src joins opts]
  (let [items (atom (mapv vector src))]
    (doseq [j joins]
      (let [joined (atom [])]
        (if (and (:leftKey j) (:rightKey j))
          (let [idx (group-by (fn [it] (apply (:rightKey j) [it])) (:items j))
                seen (atom #{})]
            (doseq [left @items]
              (let [k (apply (:leftKey j) left)
                    rs (get idx k)]
                (if rs
                  (do
                    (swap! seen conj k)
                    (doseq [right rs]
                      (swap! joined conj (conj left right))))
                  (when (:left j)
                    (swap! joined conj (conj left nil)))))
            (when (:right j)
              (doseq [right (:items j)]
                (when-not (contains? @seen (apply (:rightKey j) [right]))
                  (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right]))))))
            (reset! items @joined))
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
                (reset! items @joined)))))
    (let [it @items
          it (if-let [w (:where opts)] (vec (filter #(apply w %) it)) it)
          it (if-let [sk (:sortKey opts)]
               (vec (sort-by #(let [k (apply sk %)] (_sort_key k)) it))
               it)
          it (if (contains? opts :skip) (vec (drop (:skip opts) it)) it)
          it (if (contains? opts :take) (vec (take (:take opts) it)) it)]
      (mapv #(apply (:select opts) %) it)))))))))))))
(declare customer orders lineitem cutoff segment building_customers valid_orders valid_lineitems order_line_join)

(defn test_Q3_returns_revenue_per_order_with_correct_priority []
  (assert (_equal order_line_join [{:l_orderkey 100 :revenue (+ (* 1000.0 0.95) 500.0) :o_orderdate "1995-03-14" :o_shippriority 1}]) "expect failed")
)

(defn -main []
  (def customer [{:c_custkey 1 :c_mktsegment "BUILDING"} {:c_custkey 2 :c_mktsegment "AUTOMOBILE"}]) ;; list of
  (def orders [{:o_orderkey 100 :o_custkey 1 :o_orderdate "1995-03-14" :o_shippriority 1} {:o_orderkey 200 :o_custkey 2 :o_orderdate "1995-03-10" :o_shippriority 2}]) ;; list of
  (def lineitem [{:l_orderkey 100 :l_extendedprice 1000.0 :l_discount 0.05 :l_shipdate "1995-03-16"} {:l_orderkey 100 :l_extendedprice 500.0 :l_discount 0.0 :l_shipdate "1995-03-20"} {:l_orderkey 200 :l_extendedprice 1000.0 :l_discount 0.1 :l_shipdate "1995-03-14"}]) ;; list of
  (def cutoff "1995-03-15") ;; string
  (def segment "BUILDING") ;; string
  (def building_customers (vec (->> (for [c customer :when (_equal (:c_mktsegment c) segment)] c)))) ;; list of
  (def valid_orders (vec (->> (for [o orders :when (< (compare (:o_orderdate o) cutoff) 0) c building_customers :when (_equal (:o_custkey o) (:c_custkey c))] o)))) ;; list of
  (def valid_lineitems (vec (->> (for [l lineitem :when (> (compare (:l_shipdate l) cutoff) 0)] l)))) ;; list of
  (def order_line_join (let [_src valid_orders
      _rows (_query _src [
        {:items valid_lineitems :leftKey (fn [o] (:o_orderkey o)) :rightKey (fn [l] (:l_orderkey l))}
      ] { :select (fn [o l] [o l]) })
      _groups (_group_by _rows (fn [o l] {:o_orderkey (:o_orderkey o) :o_orderdate (:o_orderdate o) :o_shippriority (:o_shippriority o)}))
      ]
  (vec (map (fn [g] {:l_orderkey (:o_orderkey (:key g)) :revenue (_sum (vec (->> (for [r (:Items g)] (* (:l_extendedprice (:l r)) (- 1 (:l_discount (:l r)))))))) :o_orderdate (:o_orderdate (:key g)) :o_shippriority (:o_shippriority (:key g))}) _groups)))) ;; list of
  (_json order_line_join)
  (test_Q3_returns_revenue_per_order_with_correct_priority)
)

(-main)
