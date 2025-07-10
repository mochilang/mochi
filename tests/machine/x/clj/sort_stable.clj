(ns main)

(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))
(declare items result)

(defn -main []
  (def items [{:n 1 :v "a"} {:n 1 :v "b"} {:n 2 :v "c"}]) ;; list of 
  (def result (vec (->> (for [i items] (:v i)) (sort-by (fn [i] (_sort_key (:n i))))))) ;; list of string
  (println result)
)

(-main)
