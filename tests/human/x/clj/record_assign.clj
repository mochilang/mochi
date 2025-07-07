(defrecord Counter [n])
(defn inc-counter [c]
  (swap! c update :n inc))
(let [c (atom (->Counter 0))]
  (inc-counter c)
  (println (:n @c)))
