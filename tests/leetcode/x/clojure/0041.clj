(defn first-missing-positive [nums]
  (let [arr (int-array nums)
        n (alength arr)]
    (loop [i 0]
      (if (< i n)
        (let [v (aget arr i)]
          (if (and (<= 1 v n) (not= (aget arr (dec v)) v))
            (let [tmp (aget arr i)]
              (aset arr i (aget arr (dec v)))
              (aset arr (dec v) tmp)
              (recur i))
            (recur (inc i))))
        (loop [j 0]
          (if (< j n)
            (if (not= (aget arr j) (inc j))
              (inc j)
              (recur (inc j)))
            (inc n)))))))

(let [lines (-> (slurp *in*) clojure.string/split-lines)]
  (when (seq lines)
    (let [vals (map #(Integer/parseInt (clojure.string/trim %)) lines)
          t (first vals)]
      (loop [tc 0 xs (rest vals) out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [n (first xs)
                nums (vec (take n (rest xs)))]
            (recur (inc tc) (drop (inc n) xs) (conj out (str (first-missing-positive nums))))))))))
