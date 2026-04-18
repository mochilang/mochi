(ns main)

(defn convert-zigzag [s num-rows]
  (if (or (<= num-rows 1) (>= num-rows (count s)))
    s
    (let [cycle (- (* 2 num-rows) 2)]
      (apply str
             (for [row (range num-rows)
                   :let [diag-step (- cycle (* 2 row))]
                   i (range row (count s) cycle)
                   piece (if (and (> row 0) (< row (dec num-rows)) (< (+ i diag-step) (count s)))
                           [(nth s i) (nth s (+ i diag-step))]
                           [(nth s i)])]
               piece)))))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))
        t (Integer/parseInt (clojure.string/trim (first lines)))]
    (loop [i 0 idx 1 out []]
      (if (= i t)
        (print (clojure.string/join "\n" out))
        (let [s (nth lines idx "")
              num-rows (Integer/parseInt (clojure.string/trim (nth lines (inc idx) "1")))]
          (recur (inc i) (+ idx 2) (conj out (convert-zigzag s num-rows))))))))

(-main)
