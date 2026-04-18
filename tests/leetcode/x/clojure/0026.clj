(require '[clojure.string :as str])

(defn remove-duplicates [nums]
  (if (empty? nums)
    []
    (loop [res [(first nums)]
           rem (rest nums)]
      (if (empty? rem)
        res
        (if (= (first rem) (last res))
          (recur res (rest rem))
          (recur (conj res (first rem)) (rest rem)))))))

(defn solve []
  (let [input (slurp *in*)
        tokens (str/split input #"\s+")
        tokens (filter seq tokens)]
    (if (seq tokens)
      (let [t (Integer/parseInt (first tokens))
            rem (rest tokens)]
        (loop [tc 0
               cur rem]
          (if (< tc t)
            (let [n (Integer/parseInt (first cur))
                  nums (map #(Integer/parseInt %) (take n (rest cur)))
                  cur (drop (inc n) cur)
                  ans (remove-duplicates nums)]
              (println (str/join " " ans))
              (recur (inc tc) cur))))))))

(solve)
