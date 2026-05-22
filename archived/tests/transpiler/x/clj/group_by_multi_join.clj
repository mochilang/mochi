(ns main)

(require 'clojure.set)

(defrecord Partsupp [part supplier cost qty])

(defrecord Suppliers [id nation])

(defrecord Nations [id name])

(def nations [{:id 1 :name "A"} {:id 2 :name "B"}])

(def suppliers [{:id 1 :nation 1} {:id 2 :nation 2}])

(def partsupp [{:part 100 :supplier 1 :cost 10 :qty 2} {:part 100 :supplier 2 :cost 20 :qty 1} {:part 200 :supplier 1 :cost 5 :qty 3}])

(def filtered (for [ps partsupp s suppliers n nations :when (and (= (:id s) (:supplier ps)) (= (:id n) (:nation s)) (= (:name n) "A"))] {:part (:part ps) :value (* (:cost ps) (:qty ps))}))

(def grouped (for [g (for [[k rows] (group-by :key (for [x filtered :let [k (:part x)]] {:key k :item x})) :let [g {:key k :items (map :item rows)}]] g)] {:part (:key g) :total (reduce + 0 (for [r (:items g)] (:value r)))}))

(defn -main []
  (println grouped))

(-main)
