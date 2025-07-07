(ns avg-builtin)

(def data [1 2 3])
(println (/ (reduce + data) (count data)))
