(ns main)

(require 'clojure.set)

(def xs [10 20 30])

(defn -main []
  (println (nth xs 1)))

(-main)
