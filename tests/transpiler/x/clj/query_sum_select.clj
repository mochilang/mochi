(ns main)

(require 'clojure.set)

(def nums [1 2 3])

(def result (reduce + 0 (for [n nums :when (> n 1)] n)))

(defn -main []
  (println result))

(-main)
