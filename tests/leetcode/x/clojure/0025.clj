(ns main)
(defn read-int [s] (Integer/parseInt s))
(defn fmt-list [xs] (str "[" (clojure.string/join "," xs) "]"))
(defn rev-groups [xs k]
  (loop [xs xs out []]
    (if (< (count xs) k)
      (concat out xs)
      (recur (drop k xs) (concat out (reverse (take k xs)))))))
(defn -main []
  (let [lines (vec (clojure.string/split-lines (slurp *in*)))]
    (when (seq lines)
      (loop [tc 0 idx 1 t (read-int (first lines)) out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [n (read-int (nth lines idx))
                arr (mapv read-int (subvec lines (inc idx) (+ idx 1 n)))
                k (read-int (nth lines (+ idx 1 n)))]
            (recur (inc tc) (+ idx n 2) t (conj out (fmt-list (rev-groups arr k))))))))))
(-main)
