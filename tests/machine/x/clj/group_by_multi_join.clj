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
(declare nations suppliers partsupp filtered grouped)

(defn -main []
  (def nations [{:id 1 :name "A"} {:id 2 :name "B"}]) ;; list of map of string to any
  (def suppliers [{:id 1 :nation 1} {:id 2 :nation 2}]) ;; list of map of string to int
  (def partsupp [{:part 100 :supplier 1 :cost 10.0 :qty 2} {:part 100 :supplier 2 :cost 20.0 :qty 1} {:part 200 :supplier 1 :cost 5.0 :qty 3}]) ;; list of map of string to any
  (def filtered (vec (->> (for [ps partsupp s suppliers :when (= (:id s) (:supplier ps)) n nations :when (= (:id n) (:nation s)) :when (= (:name n) "A")] {:part (:part ps) :value (* (:cost ps) (:qty ps))})))) ;; list of map of string to any
  (def grouped (let [_src filtered
      _rows (_query _src [

      ] { :select (fn [x] [x]) })
      _groups (_group_by _rows (fn [x] (:part x)))
  (vec (map (fn [g] {:part (:key g) :total (_sum (vec (->> (for [r g] (:value r)))))}) _groups)))) ;; list of map of string to any
  (println grouped)
)

(-main)
