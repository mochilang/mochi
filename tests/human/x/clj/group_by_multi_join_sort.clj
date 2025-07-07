(ns group-by-multi-join-sort)

(def nation [{:n_nationkey 1 :n_name "BRAZIL"}])

(def customer
  [{:c_custkey 1
    :c_name "Alice"
    :c_acctbal 100.0
    :c_nationkey 1
    :c_address "123 St"
    :c_phone "123-456"
    :c_comment "Loyal"}])

(def orders
  [{:o_orderkey 1000 :o_custkey 1 :o_orderdate "1993-10-15"}
   {:o_orderkey 2000 :o_custkey 1 :o_orderdate "1994-01-02"}])

(def lineitem
  [{:l_orderkey 1000 :l_returnflag "R" :l_extendedprice 1000.0 :l_discount 0.1}
   {:l_orderkey 2000 :l_returnflag "N" :l_extendedprice 500.0 :l_discount 0.0}])

(def start-date "1993-10-01")
(def end-date "1994-01-01")

(def rows
  (for [c customer
        o orders :when (= (:o_custkey o) (:c_custkey c))
        l lineitem :when (= (:l_orderkey l) (:o_orderkey o))
        n nation :when (= (:n_nationkey n) (:c_nationkey c))
        :when (and (>= (:o_orderdate o) start-date)
                   (< (:o_orderdate o) end-date)
                   (= (:l_returnflag l) "R"))]
    {:c c :o o :l l :n n}))

(def result
  (->> rows
       (group-by (fn [r]
                    [(:c_custkey (:c r))
                     (:c_name (:c r))
                     (:c_acctbal (:c r))
                     (:c_address (:c r))
                     (:c_phone (:c r))
                     (:c_comment (:c r))
                     (:n_name (:n r))]))
       (map (fn [[k grp]]
              (let [revenue (reduce + (map (fn [x]
                                               (* (:l_extendedprice (:l x))
                                                  (- 1 (:l_discount (:l x)))))
                                             grp))]
                {:c_custkey (nth k 0)
                 :c_name (nth k 1)
                 :revenue revenue
                 :c_acctbal (nth k 2)
                 :n_name (nth k 6)
                 :c_address (nth k 3)
                 :c_phone (nth k 4)
                 :c_comment (nth k 5)})))
       (sort-by :revenue >)))

(println result)
