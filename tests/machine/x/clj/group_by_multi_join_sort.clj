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
    (let [g (persistent! groups)
          o (persistent! order)]
      (mapv #(get g %) o))) )

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
          (let [idx (group-by (:rightKey j) (:items j))
                seen (atom #{})]
            (doseq [left @items]
              (let [k ((:leftKey j) left)
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
                (when-not (contains? @seen ((:rightKey j) right))
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
      (mapv #(apply (:select opts) %) it))))))))))))
(declare nation customer orders lineitem start_date end_date result)

(defn -main []
  (def nation [{:n_nationkey 1 :n_name "BRAZIL"}]) ;; list of 
  (def customer [{:c_custkey 1 :c_name "Alice" :c_acctbal 100.0 :c_nationkey 1 :c_address "123 St" :c_phone "123-456" :c_comment "Loyal"}]) ;; list of 
  (def orders [{:o_orderkey 1000 :o_custkey 1 :o_orderdate "1993-10-15"} {:o_orderkey 2000 :o_custkey 1 :o_orderdate "1994-01-02"}]) ;; list of 
  (def lineitem [{:l_orderkey 1000 :l_returnflag "R" :l_extendedprice 1000.0 :l_discount 0.1} {:l_orderkey 2000 :l_returnflag "N" :l_extendedprice 500.0 :l_discount 0.0}]) ;; list of 
  (def start_date "1993-10-01") ;; string
  (def end_date "1994-01-01") ;; string
  (def result (let [_src customer
      _rows (_query _src [
        {:items orders :leftKey (fn [c] (:o_custkey o)) :rightKey (fn [o] (:c_custkey c))}
        {:items lineitem :leftKey (fn [c o] (:l_orderkey l)) :rightKey (fn [l] (:o_orderkey o))}
        {:items nation :leftKey (fn [c o l] (:n_nationkey n)) :rightKey (fn [n] (:c_nationkey c))}
      ] { :select (fn [c o l n] [c o l n]) :where (fn [c o l n] (and (and (>= (compare (:o_orderdate o) start_date) 0) (< (compare (:o_orderdate o) end_date) 0)) (= (:l_returnflag l) "R"))) })
      _groups (_group_by _rows (fn [c o l n] {:c_custkey (:c_custkey c) :c_name (:c_name c) :c_acctbal (:c_acctbal c) :c_address (:c_address c) :c_phone (:c_phone c) :c_comment (:c_comment c) :n_name (:n_name n)}))
      ]
  (vec (map (fn [g] {:c_custkey (:c_custkey (:key g)) :c_name (:c_name (:key g)) :revenue (_sum (vec (->> (for [x (:Items g)] (* (:l_extendedprice (:l x)) (- 1 (:l_discount (:l x)))))))) :c_acctbal (:c_acctbal (:key g)) :n_name (:n_name (:key g)) :c_address (:c_address (:key g)) :c_phone (:c_phone (:key g)) :c_comment (:c_comment (:key g))}) _groups)))) ;; list of map of string to any
  (println result)
)

(-main)
