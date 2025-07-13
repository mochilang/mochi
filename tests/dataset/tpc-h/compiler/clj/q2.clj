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

(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))
(declare region nation supplier part partsupp europe_nations europe_suppliers target_parts target_partsupp costs min_cost result)

(defn test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part []
  (assert (_equal result [{:s_acctbal 1000.0 :s_name "BestSupplier" :n_name "FRANCE" :p_partkey 1000 :p_mfgr "M1" :s_address "123 Rue" :s_phone "123" :s_comment "Fast and reliable" :ps_supplycost 10.0}]) "expect failed")
)

(defn -main []
  (def region [{:r_regionkey 1 :r_name "EUROPE"} {:r_regionkey 2 :r_name "ASIA"}]) ;; list of
  (def nation [{:n_nationkey 10 :n_regionkey 1 :n_name "FRANCE"} {:n_nationkey 20 :n_regionkey 2 :n_name "CHINA"}]) ;; list of
  (def supplier [{:s_suppkey 100 :s_name "BestSupplier" :s_address "123 Rue" :s_nationkey 10 :s_phone "123" :s_acctbal 1000.0 :s_comment "Fast and reliable"} {:s_suppkey 200 :s_name "AltSupplier" :s_address "456 Way" :s_nationkey 20 :s_phone "456" :s_acctbal 500.0 :s_comment "Slow"}]) ;; list of
  (def part [{:p_partkey 1000 :p_type "LARGE BRASS" :p_size 15 :p_mfgr "M1"} {:p_partkey 2000 :p_type "SMALL COPPER" :p_size 15 :p_mfgr "M2"}]) ;; list of
  (def partsupp [{:ps_partkey 1000 :ps_suppkey 100 :ps_supplycost 10.0} {:ps_partkey 1000 :ps_suppkey 200 :ps_supplycost 15.0}]) ;; list of
  (def europe_nations (vec (->> (for [r region :when (_equal (:r_name r) "EUROPE") n nation :when (_equal (:n_regionkey n) (:r_regionkey r))] n)))) ;; list of
  (def europe_suppliers (vec (->> (for [s supplier n europe_nations :when (_equal (:s_nationkey s) (:n_nationkey n))] {:s s :n n})))) ;; list of
  (def target_parts (vec (->> (for [p part :when (and (_equal (:p_size p) 15) (_equal (:p_type p) "LARGE BRASS"))] p)))) ;; list of
  (def target_partsupp (vec (->> (for [ps partsupp p target_parts :when (_equal (:ps_partkey ps) (:p_partkey p)) s europe_suppliers :when (_equal (:ps_suppkey ps) (:s_suppkey (:s s)))] {:s_acctbal (:s_acctbal (:s s)) :s_name (:s_name (:s s)) :n_name (:n_name (:n s)) :p_partkey (:p_partkey p) :p_mfgr (:p_mfgr p) :s_address (:s_address (:s s)) :s_phone (:s_phone (:s s)) :s_comment (:s_comment (:s s)) :ps_supplycost (:ps_supplycost ps)})))) ;; list of
  (def costs (vec (->> (for [x target_partsupp] (:ps_supplycost x))))) ;; list of float
  (def min_cost (apply min costs)) ;; float
  (def result (vec (->> (for [x target_partsupp :when (_equal (:ps_supplycost x) min_cost)] x) (sort-by (fn [x] (_sort_key (- (:s_acctbal x)))))))) ;; list of
  (_json result)
  (test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part)
)

(-main)
