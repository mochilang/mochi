(ns main)

(require 'clojure.set)

(def data [1 2])

(def flag (> (count (for [x data :when (= x 1)] x)) 0))

(defn -main []
  (println flag))

(-main)
