(defn hist [h]
  (loop [i 0 best 0]
    (if (= i (count h))
      best
      (let [best2 (loop [j i mn (nth h i) best best]
                    (if (= j (count h))
                      best
                      (let [mn (min mn (nth h j))
                            area (* mn (inc (- j i)))
                            best (max best area)]
                        (recur (inc j) mn best))))]
        (recur (inc i) best2)))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (let [t (Integer/parseInt (lines 0))]
      (loop [tc 0 idx 1 out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [[rows cols] (map #(Integer/parseInt %) (clojure.string/split (lines idx) #"\s+"))
                res (loop [r 0 idx (inc idx) h (vec (repeat cols 0)) best 0]
                      (if (= r rows)
                        [best idx]
                        (let [s (lines idx)
                              h2 (vec (map-indexed (fn [c v] (if (= (.charAt s c) \1) (inc v) 0)) h))]
                          (recur (inc r) (inc idx) h2 (max best (hist h2))))))]
            (recur (inc tc) (second res) (conj out (str (first res))))))))))
