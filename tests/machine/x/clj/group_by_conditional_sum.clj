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

(declare items result)

(defn -main []
  (def items [{:cat "a" :val 10 :flag true} {:cat "a" :val 5 :flag false} {:cat "b" :val 20 :flag true}]) ;; list of map of string to any
  (def result (let [_src items
      _groups (_group_by _src (fn [i] (:cat i)))
      ]
  (->> _groups (sort-by (fn [g] (:key g))) (map (fn [g] {:cat (:key g) :share (/ (_sum (vec (->> (for [x (:Items g)] (if (:flag x) (:val x) 0))))) (_sum (vec (->> (for [x (:Items g)] (:val x))))))})) vec))) ;; list of map of string to any
  (println result)
)

(-main)
