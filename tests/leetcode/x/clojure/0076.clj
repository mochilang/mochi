(defn min-window [s t]
  (let [need (int-array 128)]
    (doseq [ch t]
      (aset need (int ch) (inc (aget need (int ch)))))
    (loop [right 0 left 0 missing (count t) best-start 0 best-len (inc (count s))]
      (if (= right (count s))
        (if (> best-len (count s)) "" (subs s best-start (+ best-start best-len)))
        (let [c (int (.charAt s right))
              missing (if (pos? (aget need c)) (dec missing) missing)]
          (aset need c (dec (aget need c)))
          (let [[left missing best-start best-len]
                (loop [left left missing missing best-start best-start best-len best-len]
                  (if (zero? missing)
                    (let [window-len (- (inc right) left)
                          best-start (if (< window-len best-len) left best-start)
                          best-len (if (< window-len best-len) window-len best-len)
                          lc (int (.charAt s left))]
                      (aset need lc (inc (aget need lc)))
                      (recur (inc left)
                             (if (pos? (aget need lc)) (inc missing) missing)
                             best-start
                             best-len))
                    [left missing best-start best-len]))]
            (recur (inc right) left missing best-start best-len)))))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (let [t (Integer/parseInt (lines 0))]
      (print
       (clojure.string/join
        "\n"
        (for [i (range t)]
          (min-window (lines (inc (* 2 i))) (lines (+ 2 (* 2 i))))))))))
