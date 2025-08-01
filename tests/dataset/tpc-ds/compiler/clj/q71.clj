; Generated by Mochi compiler v0.10.25 on 2025-07-15T04:46:54Z
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
(declare item time_dim date_dim web_sales catalog_sales store_sales month year union_sales result)

(defn test_TPCDS_Q71_simplified []
  (assert (_equal result [{:i_brand_id 10 :i_brand "BrandA" :t_hour 18 :t_minute 0 :ext_price 200.0} {:i_brand_id 20 :i_brand "BrandB" :t_hour 8 :t_minute 30 :ext_price 150.0} {:i_brand_id 10 :i_brand "BrandA" :t_hour 8 :t_minute 30 :ext_price 100.0}]) "expect failed")
)

(defn -main []
  (def item [{:i_item_sk 1 :i_brand_id 10 :i_brand "BrandA" :i_manager_id 1} {:i_item_sk 2 :i_brand_id 20 :i_brand "BrandB" :i_manager_id 1}]) ;; list of
  (def time_dim [{:t_time_sk 1 :t_hour 8 :t_minute 30 :t_meal_time "breakfast"} {:t_time_sk 2 :t_hour 18 :t_minute 0 :t_meal_time "dinner"} {:t_time_sk 3 :t_hour 12 :t_minute 0 :t_meal_time "lunch"}]) ;; list of
  (def date_dim [{:d_date_sk 1 :d_moy 12 :d_year 1998}]) ;; list of
  (def web_sales [{:ws_ext_sales_price 100.0 :ws_sold_date_sk 1 :ws_item_sk 1 :ws_sold_time_sk 1}]) ;; list of
  (def catalog_sales [{:cs_ext_sales_price 200.0 :cs_sold_date_sk 1 :cs_item_sk 1 :cs_sold_time_sk 2}]) ;; list of
  (def store_sales [{:ss_ext_sales_price 150.0 :ss_sold_date_sk 1 :ss_item_sk 2 :ss_sold_time_sk 1}]) ;; list of
  (def month 12) ;; int
  (def year 1998) ;; int
  (def union_sales (concat (vec (->> (for [ws web_sales d date_dim :when (_equal (:d_date_sk d) (:ws_sold_date_sk ws)) :when (and (_equal (:d_moy d) month) (_equal (:d_year d) year))] {:ext_price (:ws_ext_sales_price ws) :item_sk (:ws_item_sk ws) :time_sk (:ws_sold_time_sk ws)}))) (vec (->> (for [cs catalog_sales d date_dim :when (_equal (:d_date_sk d) (:cs_sold_date_sk cs)) :when (and (_equal (:d_moy d) month) (_equal (:d_year d) year))] {:ext_price (:cs_ext_sales_price cs) :item_sk (:cs_item_sk cs) :time_sk (:cs_sold_time_sk cs)}))) (vec (->> (for [ss store_sales d date_dim :when (_equal (:d_date_sk d) (:ss_sold_date_sk ss)) :when (and (_equal (:d_moy d) month) (_equal (:d_year d) year))] {:ext_price (:ss_ext_sales_price ss) :item_sk (:ss_item_sk ss) :time_sk (:ss_sold_time_sk ss)}))))) ;; list of any
  (def result (let [_src item
      _rows (_query _src [
        {:items union_sales :leftKey (fn [i] (:i_item_sk i)) :rightKey (fn [s] (:item_sk s))}
        {:items time_dim :leftKey (fn [i s] (:time_sk s)) :rightKey (fn [t] (:t_time_sk t))}
      ] { :select (fn [i s t] [i s t]) :where (fn [i s t] (and (_equal (:i_manager_id i) 1) (or (_equal (:t_meal_time t) "breakfast") (_equal (:t_meal_time t) "dinner")))) })
      _groups (_group_by _rows (fn [i s t] {:brand_id (:i_brand_id i) :brand (:i_brand i) :t_hour (:t_hour t) :t_minute (:t_minute t)}))
      ]
  (vec (map (fn [g] {:i_brand_id (:brand_id (:key g)) :i_brand (:brand (:key g)) :t_hour (:t_hour (:key g)) :t_minute (:t_minute (:key g)) :ext_price (_sum (vec (->> (for [x (:Items g)] (:ext_price (:s x))))))}) _groups)))) ;; list of
  (_json result)
  (test_TPCDS_Q71_simplified)
)

(-main)
