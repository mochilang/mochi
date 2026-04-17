(ns main)

(defn my-atoi [s]
  (let [n (count s)]
    (loop [i 0]
      (if (and (< i n) (= (nth s i) \space))
        (recur (inc i))
        (let [[sign i] (if (and (< i n) (or (= (nth s i) \+) (= (nth s i) \-)))
                         [(if (= (nth s i) \-) -1 1) (inc i)]
                         [1 i])
              limit (if (= sign 1) 7 8)]
          (loop [j i ans 0]
            (if (and (< j n) (Character/isDigit ^char (nth s j)))
              (let [digit (- (int (nth s j)) 48)]
                (if (or (> ans 214748364) (and (= ans 214748364) (> digit limit)))
                  (if (= sign 1) 2147483647 -2147483648)
                  (recur (inc j) (+ (* ans 10) digit))))
              (* sign ans))))))))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))
        t (Integer/parseInt (clojure.string/trim (first lines)))]
    (print (clojure.string/join "
" (for [i (range t)] (str (my-atoi (nth lines (inc i) ""))))))))

(-main)
