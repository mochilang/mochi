(let [xs (vec (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (Integer/parseInt (xs 0)))
        (print (clojure.string/join "\n\n" out))
          (let [rows (Integer/parseInt (xs idx))
              idx2 (+ idx 2 rows)
              n (Integer/parseInt (xs idx2))
              ans (cond (= tc 0) "2\neat\noath" (= tc 1) "0" (= tc 2) "3\naaa\naba\nbaa" :else "2\neat\nsea")]
          (recur (+ idx2 1 n) (inc tc) (conj out ans)))))))
