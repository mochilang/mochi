(ns main)

(require 'clojure.set)

(def square (fn [x] (* x x)))

(defn -main []
  (println (square 6)))

(-main)
