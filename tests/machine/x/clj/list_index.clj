(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(declare xs)

(defn -main []
  (def xs [10 20 30]) ;; list of int
  (println (_indexList xs 1))
)

(-main)
