(defn is-match [s p]
  (loop [i 0 j 0 star -1 mt 0]
    (cond
      (and (< i (count s)) (< j (count p)) (or (= (nth p j) \?) (= (nth p j) (nth s i)))) (recur (inc i) (inc j) star mt)
      (and (< j (count p)) (= (nth p j) \*)) (recur i (inc j) j i)
      (and (not= star -1) (< i (count s))) (recur (inc mt) (inc star) star (inc mt))
      (>= i (count s)) (every? #(= % \*) (drop j p))
      :else false)))

(let [lines (vec (clojure.string/split (slurp *in*) #"\r?\n"))]
  (when (and (seq lines) (not= (first lines) ""))
    (loop [tc 0 idx 1 t (Integer/parseInt (clojure.string/trim (first lines))) out []]
      (if (= tc t)
        (print (clojure.string/join "\n" out))
        (let [n (Integer/parseInt (clojure.string/trim (lines idx)))
              idx1 (inc idx)
              s (if (> n 0) (lines idx1) "")
              idx2 (if (> n 0) (inc idx1) idx1)
              m (Integer/parseInt (clojure.string/trim (lines idx2)))
              idx3 (inc idx2)
              p (if (> m 0) (lines idx3) "")
              idx4 (if (> m 0) (inc idx3) idx3)]
          (recur (inc tc) idx4 t (conj out (if (is-match s p) "true" "false"))))))))
