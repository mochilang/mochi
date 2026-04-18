(ns main)

(defn max-profit [prices]
  (loop [i 1 best 0]
    (if (>= i (count prices))
      best
      (recur (inc i) (if (> (nth prices i) (nth prices (dec i)))
                       (+ best (- (nth prices i) (nth prices (dec i))))
                       best)))))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))]
    (when (seq lines)
      (let [t (Integer/parseInt (first lines))]
        (loop [tc 0 idx 1 out []]
          (if (= tc t)
            (print (clojure.string/join "\n" out))
            (let [n (Integer/parseInt (nth lines idx))
                  vals (mapv #(Integer/parseInt %) (subvec (vec lines) (inc idx) (+ idx 1 n)))]
              (recur (inc tc) (+ idx 1 n) (conj out (str (max-profit vals)))))))))))

(-main)
