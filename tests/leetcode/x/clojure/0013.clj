(ns main)

(def values {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defn roman-to-int [s]
  (reduce + (for [i (range (count s))]
              (let [cur (values (nth s i))
                    next (if (< (inc i) (count s)) (values (nth s (inc i))) 0)]
                (if (< cur next) (- cur) cur)))))

(let [tokens (clojure.string/split (slurp *in*) #"\s+")
      vals (vec (filter seq tokens))]
  (when (seq vals)
    (let [t (Integer/parseInt (first vals))]
      (doseq [s (take t (rest vals))]
        (println (roman-to-int s))))))
