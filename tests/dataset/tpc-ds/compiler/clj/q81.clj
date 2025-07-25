; Generated by Mochi compiler v0.10.25 on 2025-07-15T04:47:13Z
(ns main)

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

(declare catalog_returns avg_list avg_state result_list result)

(defn test_TPCDS_Q81_sample []
  (assert (_equal result 81.0) "expect failed")
)

(defn -main []
  (def catalog_returns [{:cust 1 :state "CA" :amt 40.0} {:cust 2 :state "CA" :amt 50.0} {:cust 3 :state "CA" :amt 81.0} {:cust 4 :state "TX" :amt 30.0} {:cust 5 :state "TX" :amt 20.0}]) ;; list of
  (def avg_list (map (fn [g] {:state (:key g) :avg_amt (let [xs (vec (->> (for [x (:Items g)] (:amt x))))] (if (empty? xs) 0 (/ (reduce + xs) (double (count xs)))))}) (_group_by catalog_returns (fn [r] (:state r))))) ;; list of
  (def avg_state (first (vec (->> (for [a avg_list :when (_equal (:state a) "CA")] a))))) ;;
  (def result_list (vec (->> (for [r catalog_returns :when (and (_equal (:state r) "CA") (> (:amt r) (* (:avg_amt avg_state) 1.2)))] (:amt r))))) ;; list of float
  (def result (first result_list)) ;; float
  (_json result)
  (test_TPCDS_Q81_sample)
)

(-main)
