(ns in-operator)

(def xs [1 2 3])
(println (boolean (some #{2} xs)))
(println (not (boolean (some #{5} xs))))
