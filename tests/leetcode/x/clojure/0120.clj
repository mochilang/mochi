(defn solve [tri]
  (let [dp (atom (vec (last tri)))]
    (doseq [i (range (- (count tri) 2) -1 -1)]
      (doseq [j (range (inc i))]
        (swap! dp assoc j (+ ((tri i) j) (min (@dp j) (@dp (inc j)))))))
    (@dp 0)))

(let [xs (mapv #(Integer/parseInt %) (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (xs 0))
        (print (clojure.string/join "\n" out))
        (let [rows (xs idx)
              [tri idx2] (loop [r 1 idx (inc idx) tri []]
                           (if (> r rows) [tri idx]
                             (recur (inc r) (+ idx r) (conj tri (subvec xs idx (+ idx r))))))]
          (recur idx2 (inc tc) (conj out (str (solve tri)))))))))
