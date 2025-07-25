; Generated by Mochi compiler v0.10.25 on 2025-07-15T04:45:29Z
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
(declare catalog_sales date_dim customer_address call_center catalog_returns filtered)

(defn CatalogSale [cs_order_number cs_ship_date_sk cs_ship_addr_sk cs_call_center_sk cs_warehouse_sk cs_ext_ship_cost cs_net_profit]
  {:__name "CatalogSale" :cs_order_number cs_order_number :cs_ship_date_sk cs_ship_date_sk :cs_ship_addr_sk cs_ship_addr_sk :cs_call_center_sk cs_call_center_sk :cs_warehouse_sk cs_warehouse_sk :cs_ext_ship_cost cs_ext_ship_cost :cs_net_profit cs_net_profit}
)


(defn DateDim [d_date_sk d_date]
  {:__name "DateDim" :d_date_sk d_date_sk :d_date d_date}
)


(defn CustomerAddress [ca_address_sk ca_state]
  {:__name "CustomerAddress" :ca_address_sk ca_address_sk :ca_state ca_state}
)


(defn CallCenter [cc_call_center_sk cc_county]
  {:__name "CallCenter" :cc_call_center_sk cc_call_center_sk :cc_county cc_county}
)


(defn CatalogReturn [cr_order_number]
  {:__name "CatalogReturn" :cr_order_number cr_order_number}
)


;; Function distinct takes [xs: list of any] and returns list of any
(defn distinct [xs]
  (try
    (def out []) ;; list of any
    (loop [_tmp0 (seq xs)]
      (when _tmp0
        (let [x (clojure.core/first _tmp0)]
          (let [r (try
            (when (not (contains out x))
              (def out (conj out x)) ;; list of any
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
(throw (ex-info "return" {:value out}))
(catch clojure.lang.ExceptionInfo e
(if (= (.getMessage e) "return")
  (:value (ex-data e))
(throw e)))
)
)

(defn test_TPCDS_Q16_shipping []
(assert (_equal filtered [{:order_count 1 :total_shipping_cost 5.0 :total_net_profit 20.0}]) "expect failed")
)

(defn -main []
(def catalog_sales [{:cs_order_number 1 :cs_ship_date_sk 1 :cs_ship_addr_sk 1 :cs_call_center_sk 1 :cs_warehouse_sk 1 :cs_ext_ship_cost 5.0 :cs_net_profit 20.0} {:cs_order_number 1 :cs_ship_date_sk 1 :cs_ship_addr_sk 1 :cs_call_center_sk 1 :cs_warehouse_sk 2 :cs_ext_ship_cost 0.0 :cs_net_profit 0.0}]) ;; list of
(def date_dim [{:d_date_sk 1 :d_date "2000-03-01"}]) ;; list of
(def customer_address [{:ca_address_sk 1 :ca_state "CA"}]) ;; list of
(def call_center [{:cc_call_center_sk 1 :cc_county "CountyA"}]) ;; list of
(def catalog_returns []) ;; list of any
(def filtered (let [_src catalog_sales
      _rows (_query _src [
        {:items date_dim :on (fn [cs1 d] (and (and (_equal (:cs_ship_date_sk cs1) (:d_date_sk d)) (>= (compare (:d_date d) "2000-03-01") 0)) (<= (compare (:d_date d) "2000-04-30") 0)))}
        {:items customer_address :on (fn [cs1 d ca] (and (_equal (:cs_ship_addr_sk cs1) (:ca_address_sk ca)) (_equal (:ca_state ca) "CA")))}
        {:items call_center :on (fn [cs1 d ca cc] (and (_equal (:cs_call_center_sk cs1) (:cc_call_center_sk cc)) (_equal (:cc_county cc) "CountyA")))}
      ] { :select (fn [cs1 d ca cc] [cs1 d ca cc]) :where (fn [cs1 d ca cc] (and (boolean (seq (vec (->> (for [cs2 catalog_sales :when (and (_equal (:cs_order_number cs1) (:cs_order_number cs2)) (not (_equal (:cs_warehouse_sk cs1) (:cs_warehouse_sk cs2))))] cs2))))) (_equal (boolean (seq (vec (->> (for [cr catalog_returns :when (_equal (:cs_order_number cs1) (:cr_order_number cr))] cr))))) false))) })
      _groups (_group_by _rows (fn [cs1 d ca cc] {}))
      ]
  (vec (map (fn [g] {:order_count (count (distinct (vec (->> (for [x (:Items g)] (:cs_order_number x)))))) :total_shipping_cost (reduce + 0 (vec (->> (for [x (:Items g)] (:cs_ext_ship_cost x))))) :total_net_profit (reduce + 0 (vec (->> (for [x (:Items g)] (:cs_net_profit x)))))}) _groups)))) ;; list of
(_json filtered)
(test_TPCDS_Q16_shipping)
)

(-main)
