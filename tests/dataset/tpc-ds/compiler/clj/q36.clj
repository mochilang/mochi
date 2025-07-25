; Generated by Mochi compiler v0.10.25 on 2025-07-15T04:46:02Z
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
(declare store_sales item store date_dim result)

(defn test_TPCDS_Q36_simplified []
  (assert (_equal result [{:i_category "Books" :i_class "C1" :gross_margin 0.2} {:i_category "Books" :i_class "C2" :gross_margin 0.25} {:i_category "Electronics" :i_class "C3" :gross_margin 0.2}]) "expect failed")
)

(defn -main []
  (def store_sales [{:ss_item_sk 1 :ss_store_sk 1 :ss_sold_date_sk 1 :ss_ext_sales_price 100.0 :ss_net_profit 20.0} {:ss_item_sk 2 :ss_store_sk 1 :ss_sold_date_sk 1 :ss_ext_sales_price 200.0 :ss_net_profit 50.0} {:ss_item_sk 3 :ss_store_sk 2 :ss_sold_date_sk 1 :ss_ext_sales_price 150.0 :ss_net_profit 30.0}]) ;; list of
  (def item [{:i_item_sk 1 :i_category "Books" :i_class "C1"} {:i_item_sk 2 :i_category "Books" :i_class "C2"} {:i_item_sk 3 :i_category "Electronics" :i_class "C3"}]) ;; list of
  (def store [{:s_store_sk 1 :s_state "A"} {:s_store_sk 2 :s_state "B"}]) ;; list of
  (def date_dim [{:d_date_sk 1 :d_year 2000}]) ;; list of
  (def result (let [_src store_sales
      _rows (_query _src [
        {:items date_dim :leftKey (fn [ss] (:ss_sold_date_sk ss)) :rightKey (fn [d] (:d_date_sk d))}
        {:items item :leftKey (fn [ss d] (:ss_item_sk ss)) :rightKey (fn [i] (:i_item_sk i))}
        {:items store :leftKey (fn [ss d i] (:ss_store_sk ss)) :rightKey (fn [s] (:s_store_sk s))}
      ] { :select (fn [ss d i s] [ss d i s]) :where (fn [ss d i s] (and (_equal (:d_year d) 2000) (or (_equal (:s_state s) "A") (_equal (:s_state s) "B")))) })
      _groups (_group_by _rows (fn [ss d i s] {:category (:i_category i) :class (:i_class i)}))
      ]
  (vec (map (fn [g] {:i_category (:category (:key g)) :i_class (:class (:key g)) :gross_margin (/ (reduce + 0 (vec (->> (for [x (:Items g)] (:ss_net_profit x))))) (reduce + 0 (vec (->> (for [x (:Items g)] (:ss_ext_sales_price x))))))}) _groups)))) ;; list of
  (_json result)
  (test_TPCDS_Q36_simplified)
)

(-main)
