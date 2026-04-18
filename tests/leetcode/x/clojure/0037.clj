(ns main)
(defn valid? [b r c ch]
  (and (every? #(and (not= (get-in b [r %]) ch) (not= (get-in b [% c]) ch)) (range 9))
       (every? #(every? (fn [j] (not= (get-in b [% j]) ch)) (range (* 3 (quot c 3)) (+ (* 3 (quot c 3)) 3))) (range (* 3 (quot r 3)) (+ (* 3 (quot r 3)) 3)))))
(defn solve [b]
  (if-let [[r c] (first (for [r (range 9) c (range 9) :when (= (get-in b [r c]) \.)] [r c]))]
    (some (fn [ch] (when (valid? b r c ch) (solve (assoc-in b [r c] ch)))) "123456789")
    b))
(defn -main []
  (let [lines (vec (clojure.string/split-lines (slurp *in*)))]
    (when (seq lines)
      (loop [tc 0 idx 1 t (Integer/parseInt (first lines)) out []]
        (if (= tc t)
          (print (clojure.string/join "\n" out))
          (let [b (mapv vec (subvec lines idx (+ idx 9))) s (solve b)]
            (recur (inc tc) (+ idx 9) t (into out (map #(apply str %) s)))))))))
(-main)
