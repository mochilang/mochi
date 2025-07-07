(ns break-continue)

(def numbers [1 2 3 4 5 6 7 8 9])

(loop [xs numbers]
  (when-let [n (first xs)]
    (cond
      (even? n) (recur (rest xs))
      (> n 7) nil
      :else (do (println "odd number:" n)
                (recur (rest xs))))))
