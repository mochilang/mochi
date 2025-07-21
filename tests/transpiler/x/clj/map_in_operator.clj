(ns main)

(require 'clojure.set)

(def m {1 "a" 2 "b"})

(defn -main []
  (println (contains? m 1))
  (println (contains? m 3)))

(-main)
