(ns main)

(require 'clojure.set)

(defrecord Items [cat val flag])

(def items [{:cat "a" :val 10 :flag true} {:cat "a" :val 5 :flag false} {:cat "b" :val 20 :flag true}])

(def result (for [g (sort-by (fn [g] (:key g)) (for [[k rows] (group-by :key (for [i items :let [k (:cat i)]] {:key k :item i})) :let [g {:key k :items (map :item rows)}]] g))] {:cat (:key g) :share (/ (reduce + 0 (for [x (:items g)] (if (:flag x) (:val x) 0))) (reduce + 0 (for [x (:items g)] (:val x))))}))

(defn -main []
  (println result))

(-main)
