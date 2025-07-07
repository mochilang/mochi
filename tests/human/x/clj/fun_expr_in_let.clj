(ns fun-expr-in-let)

(let [square (fn [x] (* x x))]
  (println (square 6)))
