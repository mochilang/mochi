(ns main)

(defn max-area [h]
  (loop [l 0 r (dec (count h)) best 0]
    (if (>= l r)
      best
      (let [height (min (nth h l) (nth h r))
            best (max best (* (- r l) height))]
        (if (< (nth h l) (nth h r))
          (recur (inc l) r best)
          (recur l (dec r) best))))))

(defn -main []
  (let [lines (clojure.string/split-lines (slurp *in*))]
    (when (seq lines)
      (let [t (Integer/parseInt (first lines))]
        (loop [tc 0 idx 1 out []]
          (if (= tc t)
            (print (clojure.string/join "\n" out))
            (let [n (Integer/parseInt (nth lines idx))
                  vals (mapv #(Integer/parseInt %) (subvec (vec lines) (inc idx) (+ idx 1 n)))]
              (recur (inc tc) (+ idx 1 n) (conj out (str (max-area vals)))))))))))

(-main)
