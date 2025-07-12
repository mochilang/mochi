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

(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))
(declare items grouped)

(defn -main []
  (def items [{:cat "a" :val 3} {:cat "a" :val 1} {:cat "b" :val 5} {:cat "b" :val 2}]) ;; list of 
  (def grouped (let [_src items
      _groups (_group_by _src (fn [i] (:cat i)))
      ]
  (->> _groups (sort-by (fn [g] (_sort_key (- (_sum (vec (->> (for [x (:Items g)] (:val x))))))))) (map (fn [g] {:cat (:key g) :total (_sum (vec (->> (for [x (:Items g)] (:val x)))))})) vec))) ;; list of 
  (println grouped)
)

(-main)
