(ns main)
(defn solve-case [s]
  (loop [i 0 stack [-1] best 0]
    (if (= i (count s))
      best
      (if (= (nth s i) \()
        (recur (inc i) (conj stack i) best)
        (let [stack (pop stack)]
          (if (empty? stack)
            (recur (inc i) [i] best)
            (recur (inc i) stack (max best (- i (peek stack))))))))))
(defn -main []
  (let [lines (vec (clojure.string/split-lines (slurp *in*)))]
    (when (seq lines)
      (loop [tc 0 idx 1 t (Integer/parseInt (first lines)) out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [n (Integer/parseInt (nth lines idx))
                s (if (> n 0) (nth lines (inc idx)) "")]
            (recur (inc tc) (+ idx 1 (if (> n 0) 1 0)) t (conj out (str (solve-case s))))))))))
(-main)
