(ns main)

(require 'clojure.set)

(defrecord Lineitem [l_orderkey l_returnflag l_extendedprice l_discount])

(defrecord Orders [o_orderkey o_custkey o_orderdate])

(defrecord Customer [c_custkey c_name c_acctbal c_nationkey c_address c_phone c_comment])

(defrecord Nation [n_nationkey n_name])

(def nation [{:n_nationkey 1 :n_name "BRAZIL"}])

(def customer [{:c_custkey 1 :c_name "Alice" :c_acctbal 100 :c_nationkey 1 :c_address "123 St" :c_phone "123-456" :c_comment "Loyal"}])

(def orders [{:o_orderkey 1000 :o_custkey 1 :o_orderdate "1993-10-15"} {:o_orderkey 2000 :o_custkey 1 :o_orderdate "1994-01-02"}])

(def lineitem [{:l_orderkey 1000 :l_returnflag "R" :l_extendedprice 1000 :l_discount 0.1} {:l_orderkey 2000 :l_returnflag "N" :l_extendedprice 500 :l_discount 0}])

(def start_date "1993-10-01")

(def end_date "1994-01-01")

(def result (for [g (sort-by (fn [g] (- (reduce + 0 (for [x (:items g)] (* (:l_extendedprice (:l x)) (- 1 (:l_discount (:l x)))))))) (for [[k rows] (group-by :key (for [c customer o orders l lineitem n nation :when (and (= (:o_custkey o) (:c_custkey c)) (= (:l_orderkey l) (:o_orderkey o)) (= (:n_nationkey n) (:c_nationkey c)) (= (and (neg? (compare (and (>= (compare (:o_orderdate o) start_date) 0) (:o_orderdate o)) end_date)) (:l_returnflag l)) "R")) :let [k {:c_custkey (:c_custkey c) :c_name (:c_name c) :c_acctbal (:c_acctbal c) :c_address (:c_address c) :c_phone (:c_phone c) :c_comment (:c_comment c) :n_name (:n_name n)}]] {:key k :item {:c c :o o :l l :n n}})) :let [g {:key k :items (map :item rows)}]] g))] {:c_custkey (:c_custkey (:key g)) :c_name (:c_name (:key g)) :revenue (reduce + 0 (for [x (:items g)] (* (:l_extendedprice (:l x)) (- 1 (:l_discount (:l x)))))) :c_acctbal (:c_acctbal (:key g)) :n_name (:n_name (:key g)) :c_address (:c_address (:key g)) :c_phone (:c_phone (:key g)) :c_comment (:c_comment (:key g))}))

(defn -main []
  (println result))

(-main)
