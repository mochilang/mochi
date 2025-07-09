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

(declare items grouped)

(defn -main []
  (def items [{:cat "a" :val 3} {:cat "a" :val 1} {:cat "b" :val 5} {:cat "b" :val 2}]) ;; list of map of string to any
  (def grouped (let [_src items
      _groups (_group_by _src (fn [i] (:cat i)))
      ]
  (->> _groups (sort-by (fn [g] (- (_sum (vec (->> (for [x (:Items g)] (:val x)))))))) (map (fn [g] {:cat (:key g) :total (_sum (vec (->> (for [x (:Items g)] (:val x)))))})) vec))) ;; list of map of string to any
  (println grouped)
)

(-main)
