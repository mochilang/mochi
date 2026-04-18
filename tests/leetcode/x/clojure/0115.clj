(ns main)

(defn solve [s t]
  (let [n (count t)]
    (loop [i 0 dp (assoc (vec (repeat (inc n) 0)) 0 1)]
      (if (= i (count s))
        (nth dp n)
        (recur (inc i)
               (loop [j n cur dp]
                 (if (= j 0)
                   cur
                   (recur (dec j)
                          (if (= (nth s i) (nth t (dec j)))
                            (assoc cur j (+ (nth cur j) (nth cur (dec j))))
                            cur)))))))))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [i 0 out []]
          (if (= i tc)
            (print (clojure.string/join "\n" out))
            (recur (inc i) (conj out (str (solve (nth lines (+ 1 (* 2 i))) (nth lines (+ 2 (* 2 i)))))))))))))

(-main)
