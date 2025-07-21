(ns main)

(require 'clojure.set)

(def nums [3 1 4])

(defn -main []
  (println (apply min nums))
  (println (apply max nums)))

(-main)
