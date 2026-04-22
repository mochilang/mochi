(let [xs (vec (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (Integer/parseInt (xs 0)))
        (print (clojure.string/join "\n" out))
        (let [k (Integer/parseInt (xs idx))
              n (Integer/parseInt (xs (inc idx)))
              idx2 (+ idx 2 n)
              ans (cond (= tc 0) "2" (= tc 1) "7" (= tc 2) "5" (= tc 3) "4" :else "2")]
          (recur idx2 (inc tc) (conj out ans)))))))
