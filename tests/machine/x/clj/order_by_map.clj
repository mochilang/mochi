(ns main)

(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))
(declare data sorted)

(defn -main []
  (def data [{:a 1 :b 2} {:a 1 :b 1} {:a 0 :b 5}]) ;; list of map of string to int
  (def sorted (vec (->> (for [x data] x) (sort-by (fn [x] (_sort_key {:a (:a x) :b (:b x)})))))) ;; list of map of string to int
  (println sorted)
)

(-main)
