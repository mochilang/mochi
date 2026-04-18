(defn solve [s1 s2 s3]
  (let [m (count s1) n (count s2)]
    (if (not= (+ m n) (count s3))
      false
      (let [dp (make-array Boolean/TYPE (inc m) (inc n))]
        (aset dp 0 0 true)
        (doseq [i (range (inc m))
                j (range (inc n))]
          (when (and (> i 0) (aget dp (dec i) j) (= (.charAt s1 (dec i)) (.charAt s3 (dec (+ i j)))))
            (aset dp i j true))
          (when (and (> j 0) (aget dp i (dec j)) (= (.charAt s2 (dec j)) (.charAt s3 (dec (+ i j)))))
            (aset dp i j true)))
        (aget dp m n)))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (let [t (Integer/parseInt (lines 0))]
      (print (clojure.string/join "\n" (for [i (range t)] (if (solve (lines (inc (* 3 i))) (lines (+ 2 (* 3 i))) (lines (+ 3 (* 3 i)))) "true" "false")))))))
