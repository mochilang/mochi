(ns main)

(require 'clojure.set)

(def a [1 2])

(defn -main []
  (println (conj a 3)))

(-main)
