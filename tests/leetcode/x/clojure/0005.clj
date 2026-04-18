(ns main)

(defn expand [s left right]
  (loop [l left r right]
    (if (and (>= l 0) (< r (count s)) (= (nth s l) (nth s r)))
      (recur (dec l) (inc r))
      [(inc l) (- r l 1)])))

(defn longest-palindrome [s]
  (loop [i 0 best-start 0 best-len (if (empty? s) 0 1)]
    (if (= i (count s))
      (subs s best-start (+ best-start best-len))
      (let [[start1 len1] (expand s i i)
            [best-start best-len] (if (> len1 best-len) [start1 len1] [best-start best-len])
            [start2 len2] (expand s i (inc i))
            [best-start best-len] (if (> len2 best-len) [start2 len2] [best-start best-len])]
        (recur (inc i) best-start best-len)))))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))
        t (Integer/parseInt (clojure.string/trim (first lines)))
        ss (take t (concat (rest lines) (repeat "")))]
    (print (clojure.string/join "\n" (map longest-palindrome ss)))))

(-main)
