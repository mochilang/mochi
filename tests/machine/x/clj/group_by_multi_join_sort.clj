(ns main)

(defn _sum [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "sum() expects list or group" {})))]
    (reduce + 0 lst))
  )

(defrecord _Group [key Items])

(defn _group_by [src keyfn]
  (let [groups (atom {})
        order (atom [])]
    (doseq [it src]
      (let [k (keyfn it)
            ks (str k)]
        (when-not (contains? @groups ks)
          (swap! groups assoc ks (_Group. k []))
          (swap! order conj ks))
        (swap! groups update ks (fn [g] (assoc g :Items (conj (:Items g) it)))))
    )
    (map (fn [k] (@groups k)) @order)))

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
      (mapv #(apply (:select opts) %) it)))
(declare nation customer orders lineitem start_date end_date result)

(defn -main []
  (def nation [{:n_nationkey 1 :n_name "BRAZIL"}]) ;; list of map of string to any
  (def customer [{:c_custkey 1 :c_name "Alice" :c_acctbal 100.0 :c_nationkey 1 :c_address "123 St" :c_phone "123-456" :c_comment "Loyal"}]) ;; list of map of string to any
  (def orders [{:o_orderkey 1000 :o_custkey 1 :o_orderdate "1993-10-15"} {:o_orderkey 2000 :o_custkey 1 :o_orderdate "1994-01-02"}]) ;; list of map of string to any
  (def lineitem [{:l_orderkey 1000 :l_returnflag "R" :l_extendedprice 1000.0 :l_discount 0.1} {:l_orderkey 2000 :l_returnflag "N" :l_extendedprice 500.0 :l_discount 0.0}]) ;; list of map of string to any
  (def start_date "1993-10-01") ;; string
  (def end_date "1994-01-01") ;; string
  (def result (let [_src customer
      _rows (_query _src [
        {:items orders :on (fn [c o] (= (:o_custkey o) (:c_custkey c)))}
        {:items lineitem :on (fn [c o l] (= (:l_orderkey l) (:o_orderkey o)))}
        {:items nation :on (fn [c o l n] (= (:n_nationkey n) (:c_nationkey c)))}
      ] { :select (fn [c o l n] [c o l n]) :where (fn [c o l n] (and (and (>= (compare (:o_orderdate o) start_date) 0) (< (compare (:o_orderdate o) end_date) 0)) (= (:l_returnflag l) "R"))) })
      _groups (_group_by _rows (fn [c o l n] {:c_custkey (:c_custkey c) :c_name (:c_name c) :c_acctbal (:c_acctbal c) :c_address (:c_address c) :c_phone (:c_phone c) :c_comment (:c_comment c) :n_name (:n_name n)}))
  (vec (map (fn [g] {:c_custkey (:c_custkey (:key g)) :c_name (:c_name (:key g)) :revenue (_sum (vec (->> (for [x g] (* (:l_extendedprice (:l x)) (- 1 (:l_discount (:l x)))))))) :c_acctbal (:c_acctbal (:key g)) :n_name (:n_name (:key g)) :c_address (:c_address (:key g)) :c_phone (:c_phone (:key g)) :c_comment (:c_comment (:key g))}) _groups)))) ;; list of map of string to any
  (println result)
)

(-main)
