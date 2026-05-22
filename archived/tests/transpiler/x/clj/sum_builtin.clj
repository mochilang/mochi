(ns main)

(require 'clojure.set)

(defn -main []
  (println (reduce + 0 [1 2 3])))

(-main)
