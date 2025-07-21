(ns main)

(require 'clojure.set)

(defrecord Anon1 [cat val])

(def items [{:cat "a" :val 3} {:cat "a" :val 1} {:cat "b" :val 5} {:cat "b" :val 2}])

(def grouped (for [g (sort-by (fn [g] (- (reduce + 0 (for [x (:items g)] (:val x))))) (for [[k rows] (group-by :key (for [i items :let [k (:cat i)]] {:key k :item i})) :let [g {:key k :items (map :item rows)}]] g))] {:cat (:key g) :total (reduce + 0 (for [x (:items g)] (:val x)))}))

(defn -main []
  (println grouped))

(-main)
