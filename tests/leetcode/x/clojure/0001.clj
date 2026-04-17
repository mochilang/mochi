(ns main)

(defn two-sum [nums target]
  (loop [i 0]
    (if (< i (count nums))
      (let [j (loop [j (inc i)]
                (cond
                  (>= j (count nums)) nil
                  (= (+ (nth nums i) (nth nums j)) target) j
                  :else (recur (inc j))))]
        (if (nil? j)
          (recur (inc i))
          [i j]))
      [0 0])))

(let [tokens (clojure.string/split (slurp *in*) #"\s+")
      values (vec (map #(Long/parseLong %) (filter seq tokens)))]
  (when (seq values)
    (loop [tc 0
           idx 1
           t (int (nth values 0))]
      (when (< tc t)
        (let [n (int (nth values idx))
              target (int (nth values (inc idx)))
              start (+ idx 2)
              nums (subvec values start (+ start n))
              ans (two-sum nums target)]
          (println (str (first ans) " " (second ans)))
          (recur (inc tc) (+ start n) t))))))
