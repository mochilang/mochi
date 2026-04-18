(ns main)

(defn lcp [strs]
  (loop [prefix (first strs)]
    (if (every? #(.startsWith ^String % prefix) strs)
      prefix
      (recur (subs prefix 0 (dec (count prefix)))))))

(let [tokens (vec (filter seq (clojure.string/split (slurp *in*) #"\s+")))]
  (when (seq tokens)
    (loop [idx 1
           t (Integer/parseInt (tokens 0))]
      (when (> t 0)
        (let [n (Integer/parseInt (tokens idx))
              strs (subvec tokens (inc idx) (+ (inc idx) n))]
          (println (str "\"" (lcp strs) "\""))
          (recur (+ idx 1 n) (dec t)))))))
