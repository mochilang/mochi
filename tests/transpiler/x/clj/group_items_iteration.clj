(ns main)

(require 'clojure.set)

(defrecord Data [tag val])

(def data [{:tag "a" :val 1} {:tag "a" :val 2} {:tag "b" :val 3}])

(def groups (for [g (for [[k rows] (group-by :key (for [d data :let [k (:tag d)]] {:key k :item d})) :let [g {:key k :items (map :item rows)}]] g)] g))

(def tmp [])

(def result (for [r (sort-by (fn [r] (:tag r)) tmp)] r))

(defn -main []
  (doseq [g groups] (do (def total 0) (doseq [x (:items g)] (def total (+ total (:val x)))) (def tmp (conj tmp {:tag (:key g) :total total}))))
  (println result))

(-main)
