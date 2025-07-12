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

(declare nations suppliers partsupp filtered grouped)

(defn -main []
  (def nations [{:id 1 :name "A"} {:id 2 :name "B"}]) ;; list of 
  (def suppliers [{:id 1 :nation 1} {:id 2 :nation 2}]) ;; list of 
  (def partsupp [{:part 100 :supplier 1 :cost 10.0 :qty 2} {:part 100 :supplier 2 :cost 20.0 :qty 1} {:part 200 :supplier 1 :cost 5.0 :qty 3}]) ;; list of 
  (def filtered (vec (->> (for [ps partsupp s suppliers :when (_equal (:id s) (:supplier ps)) n nations :when (_equal (:id n) (:nation s)) :when (_equal (:name n) "A")] {:part (:part ps) :value (* (:cost ps) (:qty ps))})))) ;; list of 
  (def grouped (map (fn [g] {:part (:key g) :total (_sum (vec (->> (for [r (:Items g)] (:value r)))))}) (_group_by filtered (fn [x] (:part x))))) ;; list of 
  (println grouped)
)

(-main)
