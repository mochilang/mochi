(ns pure-global-fold)

(def k 2)

(defn inc-val [x]
  (+ x k))

(println (inc-val 3))
