(ns main)

(require 'clojure.set)

(defrecord Items [a b val])

(def items [{:a "x" :b 1 :val 2} {:a "x" :b 2 :val 3} {:a "y" :b 1 :val 4} {:a "y" :b 2 :val 1}])

(def grouped (for [g (sort-by (fn [g] (- (reduce + 0 (for [x (:items g)] (:val x))))) (for [[k rows] (group-by :key (for [i items :let [k {:a (:a i) :b (:b i)}]] {:key k :item i})) :let [g {:key k :items (map :item rows)}]] g))] {:a (:a (:key g)) :b (:b (:key g)) :total (reduce + 0 (for [x (:items g)] (:val x)))}))

(defn -main []
  (println grouped))

(-main)
