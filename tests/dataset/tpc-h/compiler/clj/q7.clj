; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:01:03Z
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
        (cond
          (and (:left j) (:right j))
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
                  (swap! joined conj
                    (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
              (reset! items @joined))
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
                    (swap! joined conj
                      (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
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
      (mapv #(apply (:select opts) (take (inc (count joins)) %)) it))))))))))
(declare nation supplier customer orders lineitem start_date end_date nation1 nation2 result)

(defn test_Q7_computes_revenue_between_FRANCE_and_GERMANY_by_year []
  (assert (= result [{:supp_nation "FRANCE" :cust_nation "GERMANY" :l_year "1995" :revenue 900.0}]) "expect failed")
)

(defn -main []
  (def nation [{:n_nationkey 1 :n_name "FRANCE"} {:n_nationkey 2 :n_name "GERMANY"}]) ;; list of
  (def supplier [{:s_suppkey 100 :s_nationkey 1}]) ;; list of
  (def customer [{:c_custkey 200 :c_nationkey 2}]) ;; list of
  (def orders [{:o_orderkey 1000 :o_custkey 200}]) ;; list of
  (def lineitem [{:l_orderkey 1000 :l_suppkey 100 :l_extendedprice 1000.0 :l_discount 0.1 :l_shipdate "1995-06-15"} {:l_orderkey 1000 :l_suppkey 100 :l_extendedprice 800.0 :l_discount 0.05 :l_shipdate "1997-01-01"}]) ;; list of
  (def start_date "1995-01-01") ;; string
  (def end_date "1996-12-31") ;; string
  (def nation1 "FRANCE") ;; string
  (def nation2 "GERMANY") ;; string
  (def result (let [_src lineitem
      _rows (_query _src [
        {:items orders :leftKey (fn [l] (:l_orderkey l)) :rightKey (fn [o] (:o_orderkey o))}
        {:items customer :leftKey (fn [l o] (:o_custkey o)) :rightKey (fn [c] (:c_custkey c))}
        {:items supplier :leftKey (fn [l o c] (:l_suppkey l)) :rightKey (fn [s] (:s_suppkey s))}
        {:items nation :leftKey (fn [l o c s] (:s_nationkey s)) :rightKey (fn [n1] (:n_nationkey n1))}
        {:items nation :leftKey (fn [l o c s n1] (:c_nationkey c)) :rightKey (fn [n2] (:n_nationkey n2))}
      ] { :select (fn [l o c s n1 n2] [l o c s n1 n2]) :where (fn [l o c s n1 n2] (or (and (and (>= (compare (:l_shipdate l) start_date) 0) (<= (compare (:l_shipdate l) end_date) 0)) (and (= (:n_name n1) nation1) (= (:n_name n2) nation2))) (and (= (:n_name n1) nation2) (= (:n_name n2) nation1)))) })
      _groups (_group_by _rows (fn [l o c s n1 n2] {:supp_nation (:n_name n1) :cust_nation (:n_name n2) :l_year (.substring (:l_shipdate l) 0 4)}))
      ]
  (vec (map (fn [g] {:supp_nation (:supp_nation (:key g)) :cust_nation (:cust_nation (:key g)) :l_year (:l_year (:key g)) :revenue (_sum (vec (->> (for [x (:Items g)] (* (:l_extendedprice (:l x)) (- 1 (:l_discount (:l x))))))))}) _groups)))) ;; list of map of string to any
  (_json result)
  (test_Q7_computes_revenue_between_FRANCE_and_GERMANY_by_year)
)

(-main)
