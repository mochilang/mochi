(ns main)

(defn _union_all [a b]
  (vec (concat a b)))
(defn _union [a b]
  (vec (distinct (concat a b))))
(defn _except [a b]
  (vec (remove (set b) a)))
(defn _intersect [a b]
  (vec (distinct (filter (set b) a))))
(defn -main []
  (println (_union [1 2] [2 3]))
  (println (_except [1 2 3] [2]))
  (println (_intersect [1 2 3] [2 4]))
  (println (count (_union_all [1 2] [2 3])))
)

(-main)
