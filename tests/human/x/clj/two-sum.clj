(defn two-sum [nums target]
  (let [n (count nums)]
    (loop [i 0]
      (if (< i n)
        (let [j (loop [j (inc i)]
                  (cond
                    (>= j n) nil
                    (= (+ (nth nums i) (nth nums j)) target) j
                    :else (recur (inc j))))]
          (if j
            [i j]
            (recur (inc i))))
        [-1 -1]))))

(let [[i j] (two-sum [2 7 11 15] 9)]
  (println i)
  (println j))
