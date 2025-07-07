(ns nested-function)

(defn outer [x]
  (letfn [(inner [y] (+ x y))]
    (inner 5)))

(println (outer 3))
