(ns main (:refer-clojure :exclude [sum_tree]))

(require 'clojure.set)

(defn sum_tree [t]
  (cond (= t :Leaf) 0 true (let [left (:left t) value (:value t) right (:right t)] (+ (+ (sum_tree left) value) (sum_tree right)))))

(def t {:left :Leaf :value 1 :right {:left :Leaf :value 2 :right :Leaf}})

(defn -main []
  (println (sum_tree t)))

(-main)
