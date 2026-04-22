(defn solve [dungeon]
  (let [cols (count (first dungeon))
        dp (atom (vec (concat (repeat (dec cols) 1000000000) [1 1000000000])))]
    (doseq [row (reverse dungeon)]
      (doseq [j (range (dec cols) -1 -1)]
        (let [need (- (min (@dp j) (@dp (inc j))) (row j))]
          (swap! dp assoc j (if (<= need 1) 1 need)))))
    (@dp 0)))

(let [xs (mapv #(Integer/parseInt %) (re-seq #"\S+" (slurp *in*)))]
  (when (seq xs)
    (loop [idx 1 tc 0 out []]
      (if (= tc (xs 0))
        (print (clojure.string/join "\n" out))
        (let [rows (xs idx)
              cols (xs (inc idx))
              [dungeon idx2] (loop [r 0 pos (+ idx 2) acc []]
                               (if (= r rows) [acc pos]
                                 (recur (inc r) (+ pos cols) (conj acc (subvec xs pos (+ pos cols))))))]
          (recur idx2 (inc tc) (conj out (str (solve dungeon)))))))))
