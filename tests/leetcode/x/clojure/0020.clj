(ns main)

(def pairs {\) \(, \] \[, \} \{})

(defn valid? [s]
  (loop [chars (seq s) stack []]
    (if (nil? chars)
      (empty? stack)
      (let [ch (first chars)]
        (if (contains? #{\( \[ \{} ch)
          (recur (next chars) (conj stack ch))
          (and (seq stack)
               (= (peek stack) (pairs ch))
               (recur (next chars) (pop stack))))))))

(let [tokens (clojure.string/split (slurp *in*) #"\s+")
      vals (vec (filter seq tokens))]
  (when (seq vals)
    (let [t (Integer/parseInt (first vals))]
      (doseq [s (take t (rest vals))]
        (println (if (valid? s) "true" "false"))))))
