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

(declare nations suppliers partsupp filtered grouped)

(defn -main []
  (def nations [{:id 1 :name "A"} {:id 2 :name "B"}]) ;; list of map of string to any
  (def suppliers [{:id 1 :nation 1} {:id 2 :nation 2}]) ;; list of map of string to int
  (def partsupp [{:part 100 :supplier 1 :cost 10.0 :qty 2} {:part 100 :supplier 2 :cost 20.0 :qty 1} {:part 200 :supplier 1 :cost 5.0 :qty 3}]) ;; list of map of string to any
  (def filtered (vec (->> (for [ps partsupp s suppliers :when (= (:id s) (:supplier ps)) n nations :when (= (:id n) (:nation s)) :when (= (:name n) "A")] {:part (:part ps) :value (* (:cost ps) (:qty ps))})))) ;; list of map of string to any
  (def grouped (map (fn [g] {:part (:key g) :total (_sum (vec (->> (for [r (:Items g)] (:value r)))))}) (_group_by filtered (fn [x] (:part x))))) ;; list of map of string to any
  (println grouped)
)

(-main)
