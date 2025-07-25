; Generated by Mochi compiler v0.10.28 on 2025-07-18T07:01:24Z
(ns main)

(defn _count [v]
  (cond
    (sequential? v) (count v)
    (and (map? v) (contains? v :Items)) (count (:Items v))
    :else (throw (ex-info "count() expects list or group" {}))))

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
(declare nation supplier orders lineitem result)

(defn test_Q21_returns_Saudi_suppliers_who_caused_unique_delivery_delays []
  (assert (= result [{:s_name "Desert Trade" :numwait 1}]) "expect failed")
)

(defn -main []
  (def nation [{:n_nationkey 1 :n_name "SAUDI ARABIA"} {:n_nationkey 2 :n_name "FRANCE"}]) ;; list of
  (def supplier [{:s_suppkey 100 :s_name "Desert Trade" :s_nationkey 1} {:s_suppkey 200 :s_name "Euro Goods" :s_nationkey 2}]) ;; list of
  (def orders [{:o_orderkey 500 :o_orderstatus "F"} {:o_orderkey 600 :o_orderstatus "O"}]) ;; list of
  (def lineitem [{:l_orderkey 500 :l_suppkey 100 :l_receiptdate "1995-04-15" :l_commitdate "1995-04-10"} {:l_orderkey 500 :l_suppkey 200 :l_receiptdate "1995-04-12" :l_commitdate "1995-04-12"} {:l_orderkey 600 :l_suppkey 100 :l_receiptdate "1995-05-01" :l_commitdate "1995-04-25"}]) ;; list of
  (def result (let [_src supplier
      _rows (_query _src [
        {:items lineitem :leftKey (fn [s] (:s_suppkey s)) :rightKey (fn [l1] (:l_suppkey l1))}
        {:items orders :leftKey (fn [s l1] (:l_orderkey l1)) :rightKey (fn [o] (:o_orderkey o))}
        {:items nation :leftKey (fn [s l1 o] (:s_nationkey s)) :rightKey (fn [n] (:n_nationkey n))}
      ] { :select (fn [s l1 o n] [s l1 o n]) :where (fn [s l1 o n] (and (and (and (= (:o_orderstatus o) "F") (> (compare (:l_receiptdate l1) (:l_commitdate l1)) 0)) (= (:n_name n) "SAUDI ARABIA")) (not (boolean (seq (vec (->> (for [x lineitem :when (and (and (= (:l_orderkey x) (:l_orderkey l1)) (not (= (:l_suppkey x) (:l_suppkey l1)))) (> (compare (:l_receiptdate x) (:l_commitdate x)) 0))] x)))))))) })
      _groups (_group_by _rows (fn [s l1 o n] (:s_name s)))
      ]
  (vec (map (fn [g] {:s_name (:key g) :numwait (count (:Items g))}) _groups)))) ;; list of map of string to any
  (_json result)
  (test_Q21_returns_Saudi_suppliers_who_caused_unique_delivery_delays)
)

(-main)
