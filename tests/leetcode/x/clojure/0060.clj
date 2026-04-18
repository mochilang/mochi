(defn get-permutation [n k-input]
  (let [digits (atom (vec (map str (range 1 (inc n)))))
        fact (vec (reductions * 1 (range 1 (inc n))))]
    (loop [k (dec k-input) rem n out ""]
      (if (zero? rem)
        out
        (let [block (fact (dec rem))
              idx (quot k block)
              k' (mod k block)
              pick (@digits idx)]
          (swap! digits #(vec (concat (subvec % 0 idx) (subvec % (inc idx)))))
          (recur k' (dec rem) (str out pick)))))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (loop [t (Integer/parseInt (clojure.string/trim (lines 0))) idx 1 out []]
      (if (zero? t)
        (print (clojure.string/join "\n" out))
        (let [n (Integer/parseInt (clojure.string/trim (lines idx)))
              k (Integer/parseInt (clojure.string/trim (lines (inc idx))))]
          (recur (dec t) (+ idx 2) (conj out (get-permutation n k))))))))
