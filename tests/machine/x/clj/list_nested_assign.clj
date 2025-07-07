(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(declare matrix)

(defn -main []
  (def matrix [[1 2] [3 4]]) ;; list of list of int
  (def matrix (assoc-in matrix [1 0] 5)) ;; int
  (println (_indexList (_indexList matrix 1) 0))
)

(-main)
