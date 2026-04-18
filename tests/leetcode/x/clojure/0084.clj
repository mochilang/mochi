(defn solve [a]
  (loop [i 0 best 0]
    (if (= i (count a))
      best
      (let [best2 (loop [j i mn (nth a i) best best]
                    (if (= j (count a))
                      best
                      (let [mn (min mn (nth a j))
                            area (* mn (inc (- j i)))
                            best (max best area)]
                        (recur (inc j) mn best))))]
        (recur (inc i) best2)))))

(let [xs (mapv #(Integer/parseInt %) (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (xs 0))
        (print (clojure.string/join "\n" out))
        (let [n (xs idx)
              arr (subvec xs (inc idx) (+ idx 1 n))]
          (recur (+ idx 1 n) (inc tc) (conj out (str (solve arr)))))))))
