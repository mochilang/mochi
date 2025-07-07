(ns partial-application)

(defn add [a b]
  (+ a b))

(def add5 (partial add 5))

(println (add5 3))
