(defn solve-n-queens [n]
  (let [cols (boolean-array n) d1 (boolean-array (* 2 n)) d2 (boolean-array (* 2 n)) board (object-array n) res (java.util.ArrayList.)]
    (dotimes [i n] (aset board i (char-array (repeat n \.))))
    (letfn [(row-str [r] (apply str (seq ^chars (aget board r))))
            (dfs [r]
              (if (= r n)
                (.add res (vec (map row-str (range n))))
                (dotimes [c n]
                  (let [a (+ r c) b (+ (- r c) (dec n))]
                    (when (not (or (aget cols c) (aget d1 a) (aget d2 b)))
                      (aset cols c true) (aset d1 a true) (aset d2 b true)
                      (aset ^chars (aget board r) c \Q)
                      (dfs (inc r))
                      (aset ^chars (aget board r) c \.)
                      (aset cols c false) (aset d1 a false) (aset d2 b false))))))]
      (dfs 0)
      (vec res))))

(let [lines (vec (clojure.string/split-lines (slurp *in*)))]
  (when (seq lines)
    (loop [tc 0 idx 1 t (Integer/parseInt (clojure.string/trim (lines 0))) out []]
      (if (= tc t)
        (print (clojure.string/join "\n" out))
        (let [n (Integer/parseInt (clojure.string/trim (lines idx)))
              sols (solve-n-queens n)
              block (concat [(str (count sols))] (mapcat (fn [[si sol]] (concat sol (when (< si (dec (count sols))) ["-"]))) (map-indexed vector sols)) (when (< tc (dec t)) ["="]))]
          (recur (inc tc) (inc idx) t (concat out block)))))))
