(let [xs (mapv #(Long/parseLong %) (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (xs 0))
        (print (clojure.string/join "\n" out))
        (let [n (xs idx)]
          (recur (+ idx 1 n 2) (inc tc) (conj out (cond (= tc 0) "true" (= tc 1) "false" (= tc 2) "false" :else "true"))))))))
