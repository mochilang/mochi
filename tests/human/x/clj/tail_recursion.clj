(defn sum-rec [n acc]
  (if (zero? n)
    acc
    (recur (dec n) (+ acc n))))

(println (sum-rec 10 0))
