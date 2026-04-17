(ns main)

(def int-min -2147483648)
(def int-max 2147483647)

(defn reverse-int [x]
  (loop [x x ans 0]
    (if (zero? x)
      ans
      (let [digit (rem x 10)
            x (quot x 10)]
        (cond
          (or (> ans (quot int-max 10)) (and (= ans (quot int-max 10)) (> digit 7))) 0
          (or (< ans (quot int-min 10)) (and (= ans (quot int-min 10)) (< digit -8))) 0
          :else (recur x (+ (* ans 10) digit)))))))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))
        t (Integer/parseInt (clojure.string/trim (first lines)))]
    (print (clojure.string/join "\n"
            (for [i (range t)]
              (str (reverse-int (Integer/parseInt (clojure.string/trim (nth lines (inc i) "0"))))))))) )

(-main)
