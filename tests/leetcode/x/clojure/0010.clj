(ns main)

(declare match-at)

(defn match-at [s p i j]
  (if (= j (count p))
    (= i (count s))
    (let [first (and (< i (count s)) (or (= (nth p j) \.) (= (nth s i) (nth p j))))]
      (if (and (< (inc j) (count p)) (= (nth p (inc j)) \*))
        (or (match-at s p i (+ j 2)) (and first (match-at s p (inc i) j)))
        (and first (match-at s p (inc i) (inc j)))))))

(defn -main []
  (let [lines (vec (clojure.string/split-lines (slurp *in*)))]
    (when (seq lines)
      (let [t (Integer/parseInt (first lines))]
        (loop [tc 0 idx 1 out []]
          (if (= tc t)
            (print (clojure.string/join "\n" out))
            (let [s (nth lines idx)
                  p (nth lines (inc idx))]
              (recur (inc tc) (+ idx 2) (conj out (if (match-at s p 0 0) "true" "false"))))))))))

(-main)
