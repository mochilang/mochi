(ns main)

(require 'clojure.set)

(def x 5)

(defn -main []
  (if (> x 3) (println "big") (println "small")))

(-main)
