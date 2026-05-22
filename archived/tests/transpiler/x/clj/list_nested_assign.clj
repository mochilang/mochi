(ns main)

(require 'clojure.set)

(def matrix [[1 2] [3 4]])

(defn -main []
  (def matrix (assoc-in matrix [1 0] 5))
  (println (nth (nth matrix 1) 0)))

(-main)
