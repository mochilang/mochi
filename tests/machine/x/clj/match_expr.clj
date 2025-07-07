(ns main)

(declare x label)

(defn -main []
  (def x 2) ;; int
  (def label (let [t x]
  (cond
    (= t 1) "one"
    (= t 2) "two"
    (= t 3) "three"
    :else "unknown"
  ))) ;; string
  (println label)
)

(-main)
