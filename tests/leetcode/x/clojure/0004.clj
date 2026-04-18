(ns main)

(defn median [a b]
  (let [merged (vec (sort (concat a b)))
        n (count merged)]
    (if (odd? n)
      (double (nth merged (quot n 2)))
      (/ (+ (nth merged (dec (quot n 2)))
            (nth merged (quot n 2)))
         2.0))))

(defn -main []
  (let [lines (vec (clojure.string/split-lines (slurp *in*)))]
    (when (seq lines)
      (let [t (Integer/parseInt (first lines))]
        (loop [tc 0 idx 1 out []]
          (if (= tc t)
            (print (clojure.string/join "\n" out))
            (let [n (Integer/parseInt (nth lines idx))
                  a (mapv #(Integer/parseInt %) (subvec lines (inc idx) (+ idx 1 n)))
                  idx (+ idx 1 n)
                  m (Integer/parseInt (nth lines idx))
                  b (mapv #(Integer/parseInt %) (subvec lines (inc idx) (+ idx 1 m)))]
              (recur (inc tc)
                     (+ idx 1 m)
                     (conj out (format "%.1f" (double (median a b))))))))))))

(-main)
