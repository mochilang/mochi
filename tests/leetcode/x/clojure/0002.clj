(ns main)

(defn add-lists [a b]
  (loop [a a b b carry 0 out []]
    (if (and (empty? a) (empty? b) (zero? carry))
      out
      (let [sum (+ carry (if (seq a) (first a) 0) (if (seq b) (first b) 0))]
        (recur (if (seq a) (rest a) a)
               (if (seq b) (rest b) b)
               (quot sum 10)
               (conj out (mod sum 10)))))))

(defn fmt [xs] (str "[" (clojure.string/join "," xs) "]"))

(let [tokens (clojure.string/split (slurp *in*) #"\s+")
      vals (vec (filter seq tokens))]
  (when (seq vals)
    (loop [idx 1 tc 0 t (Integer/parseInt (vals 0)) out []]
      (if (= tc t)
        (doseq [line out] (println line))
        (let [n (Integer/parseInt (vals idx))
              a (mapv #(Integer/parseInt %) (subvec vals (inc idx) (+ (inc idx) n)))
              idx2 (+ idx 1 n)
              m (Integer/parseInt (vals idx2))
              b (mapv #(Integer/parseInt %) (subvec vals (inc idx2) (+ (inc idx2) m)))]
          (recur (+ idx2 1 m) (inc tc) t (conj out (fmt (add-lists a b)))))))))
