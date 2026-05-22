(ns main)

(require 'clojure.set)

(defn -main []
  (println (count {"a" 1 "b" 2})))

(-main)
