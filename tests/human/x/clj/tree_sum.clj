(defrecord Node [left value right])

(defn sum-tree [t]
  (if (nil? t)
    0
    (+ (sum-tree (:left t)) (:value t) (sum-tree (:right t)))))

(def t (->Node nil 1 (->Node nil 2 nil)))
(println (sum-tree t))
