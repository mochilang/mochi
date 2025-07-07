(ns main)

(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

(declare nums)

(defn -main []
  (def nums [1 2]) ;; list of int
  (def nums (assoc nums 1 3)) ;; int
  (println (_indexList nums 1))
)

(-main)
