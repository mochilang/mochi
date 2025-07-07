(ns main)

(defn _count [v]
  (cond
    (sequential? v) (count v)
    (and (map? v) (contains? v :Items)) (count (:Items v))
    :else (throw (ex-info "count() expects list or group" {}))))

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
(declare people big)

(defn -main []
  (def people [{:name "Alice" :city "Paris"} {:name "Bob" :city "Hanoi"} {:name "Charlie" :city "Paris"} {:name "Diana" :city "Hanoi"} {:name "Eve" :city "Paris"} {:name "Frank" :city "Hanoi"} {:name "George" :city "Paris"}]) ;; list of map of string to string
  (def big (let [_src people
      _rows (_query _src [

      ] { :select (fn [p] [p]) })
      _groups (_group_by _rows (fn [p] (:city p)))
  (vec (map (fn [g] {:city (:key g) :num (count (:Items g))}) _groups)))) ;; list of map of string to any
  (_json big)
)

(-main)
