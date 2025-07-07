(ns closure)

(defn make-adder [n]
  (fn [x] (+ x n)))

(def add10 (make-adder 10))
(println (add10 7))
