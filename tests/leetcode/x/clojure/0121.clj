(ns main)

(defn max-profit [prices]
  (if (empty? prices)
    0
    (loop [ps (rest prices) min-price (first prices) best 0]
      (if (empty? ps)
        best
        (let [p (first ps)]
          (recur (rest ps) (min min-price p) (max best (- p min-price))))))))

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
