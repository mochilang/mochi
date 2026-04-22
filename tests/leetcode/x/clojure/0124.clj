(ns main)

(defn solve [vals ok]
  (let [best (atom -1000000000)]
    (letfn [(dfs [i]
              (if (or (>= i (count vals)) (not (nth ok i)))
                0
                (let [left (max 0 (dfs (+ (* 2 i) 1)))
                      right (max 0 (dfs (+ (* 2 i) 2)))
                      total (+ (nth vals i) left right)]
                  (swap! best max total)
                  (+ (nth vals i) (max left right)))))]
      (dfs 0)
      @best)))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [t 0 idx 1 out []]
          (if (= t tc)
            (print (clojure.string/join "\n" out))
            (let [n (Integer/parseInt (nth lines idx))
                  toks (subvec (vec lines) (inc idx) (+ idx 1 n))
                  vals (mapv #(if (= % "null") 0 (Integer/parseInt %)) toks)
                  ok (mapv #(not= % "null") toks)]
              (recur (inc t) (+ idx 1 n) (conj out (str (solve vals ok)))))))))))

(-main)
