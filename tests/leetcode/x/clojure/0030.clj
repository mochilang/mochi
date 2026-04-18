(ns main)
(defn read-int [s] (Integer/parseInt s))
(defn fmt-list [xs] (str "[" (clojure.string/join "," xs) "]"))
(defn solve-case [s words]
  (if (empty? words)
    []
    (let [wlen (count (first words)) total (* wlen (count words)) target (sort words)]
      (loop [i 0 ans []]
        (if (> (+ i total) (count s))
          ans
          (let [parts (sort (map #(subs s (+ i (* % wlen)) (+ i (* (inc %) wlen))) (range (count words))))]
            (recur (inc i) (if (= parts target) (conj ans i) ans))))))))
(defn -main []
  (let [lines (vec (clojure.string/split-lines (slurp *in*)))]
    (when (seq lines)
      (loop [tc 0 idx 1 t (read-int (first lines)) out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [s (nth lines idx) m (read-int (nth lines (inc idx))) words (subvec lines (+ idx 2) (+ idx 2 m))]
            (recur (inc tc) (+ idx 2 m) t (conj out (fmt-list (solve-case s words))))))))))
(-main)
