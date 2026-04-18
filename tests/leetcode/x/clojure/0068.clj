(defn spaces [n]
  (apply str (repeat n " ")))

(defn justify [words max-width]
  (loop [i 0 res []]
    (if (>= i (count words))
      res
      (let [[j total]
            (loop [j i total 0]
              (if (and (< j (count words))
                       (<= (+ total (count (words j)) (- j i)) max-width))
                (recur (inc j) (+ total (count (words j))))
                [j total]))
            gaps (dec (- j i))
            line (if (or (= j (count words)) (zero? gaps))
                   (let [s (clojure.string/join " " (subvec words i j))]
                     (str s (spaces (- max-width (count s)))))
                   (let [space-total (- max-width total)
                         base (quot space-total gaps)
                         extra (mod space-total gaps)]
                     (loop [k i s ""]
                       (if (= k (dec j))
                         (str s (words k))
                         (recur (inc k)
                                (str s
                                     (words k)
                                     (spaces (+ base (if (< (- k i) extra) 1 0)))))))))]
        (recur j (conj res line))))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (let [t (Integer/parseInt (lines 0))]
      (loop [tc 0 idx 1 out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [n (Integer/parseInt (lines idx))
                idx (inc idx)
                words (subvec lines idx (+ idx n))
                idx (+ idx n)
                width (Integer/parseInt (lines idx))
                idx (inc idx)
                ans (justify words width)
                block (concat [(str (count ans))]
                              (map #(str "|" % "|") ans)
                              (when (< (inc tc) t) ["="]))]
            (recur (inc tc) idx (into out block))))))))
