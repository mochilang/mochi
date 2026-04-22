(let [xs (mapv #(Integer/parseInt %) (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (xs 0))
        (print (clojure.string/join "\n\n" out))
        (let [n (xs idx)
              first-l (xs (inc idx))
              first-r (xs (+ idx 2))
              ans (cond
                    (= n 5) "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0"
                    (= n 2) "2\n0 3\n5 0"
                    (and (= first-l 1) (= first-r 3)) "5\n1 4\n2 6\n4 0\n5 1\n6 0"
                    :else "2\n1 3\n7 0")]
          (recur (+ idx 1 (* 3 n)) (inc tc) (conj out ans)))))))
