(ns main)

(require 'clojure.set)

(def m {1 "a" 2 "b"})

(defn -main []
  (println (get m 1)))

(-main)
