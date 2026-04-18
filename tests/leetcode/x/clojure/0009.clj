(ns main)

(defn is-palindrome [x]
  (if (< x 0)
    false
    (= x (loop [n x rev 0]
           (if (zero? n)
             rev
             (recur (quot n 10) (+ (* rev 10) (mod n 10))))))))

(let [tokens (clojure.string/split (slurp *in*) #"\s+")
      values (vec (map #(Long/parseLong %) (filter seq tokens)))]
  (when (seq values)
    (let [t (int (first values))]
      (doseq [x (take t (rest values))]
        (println (if (is-palindrome x) "true" "false"))))))
