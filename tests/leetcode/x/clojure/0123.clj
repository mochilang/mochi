(ns main)

(defn max-profit [prices]
  (loop [ps prices buy1 -1000000000 sell1 0 buy2 -1000000000 sell2 0]
    (if (empty? ps)
      sell2
      (let [p (first ps)
            buy1 (max buy1 (- p))
            sell1 (max sell1 (+ buy1 p))
            buy2 (max buy2 (- sell1 p))
            sell2 (max sell2 (+ buy2 p))]
        (recur (rest ps) buy1 sell1 buy2 sell2)))))

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
