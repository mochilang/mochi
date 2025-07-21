(ns main)

(require 'clojure.set)

(def x 1)

(defn -main []
  (def x 2)
  (println x))

(-main)
