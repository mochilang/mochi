(defn trap-water [h]
  (loop [left 0 right (dec (count h)) left-max 0 right-max 0 water 0]
    (if (> left right)
      water
      (if (<= left-max right-max)
        (let [v (nth h left)]
          (if (< v left-max)
            (recur (inc left) right left-max right-max (+ water (- left-max v)))
            (recur (inc left) right v right-max water)))
        (let [v (nth h right)]
          (if (< v right-max)
            (recur left (dec right) left-max right-max (+ water (- right-max v)))
            (recur left (dec right) left-max v water)))))))

(let [lines (-> (slurp *in*) clojure.string/split-lines)]
  (when (seq lines)
    (let [vals (map #(Integer/parseInt (clojure.string/trim %)) lines)
          t (first vals)]
      (loop [tc 0 xs (rest vals) out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [n (first xs) arr (vec (take n (rest xs)))]
            (recur (inc tc) (drop (inc n) xs) (conj out (str (trap-water arr))))))))))
