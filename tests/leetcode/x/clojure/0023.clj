(ns main)
(defn read-int [s] (Integer/parseInt s))
(defn fmt-list [xs] (str "[" (clojure.string/join "," xs) "]"))
(defn -main []
  (let [lines (vec (clojure.string/split-lines (slurp *in*)))]
    (when (seq lines)
      (let [t (read-int (first lines))]
        (loop [tc 0 idx 1 out []]
          (if (= tc t)
            (print (clojure.string/join "
" out))
            (let [k (read-int (nth lines idx))
                  [idx vals] (loop [i 0 idx (inc idx) vals []]
                               (if (= i k)
                                 [idx vals]
                                 (let [n (read-int (nth lines idx))
                                       xs (mapv read-int (subvec lines (inc idx) (+ idx 1 n)))]
                                   (recur (inc i) (+ idx 1 n) (into vals xs)))))]
              (recur (inc tc) idx (conj out (fmt-list (sort vals)))))))))))
(-main)
