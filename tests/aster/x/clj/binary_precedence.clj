(ns main)
(require 'clojure.set)
(defn -main [] (println (* (+ 1 2) 3)) (println (* (+ 1 2) 3)) (println (+ (* 2 3) 1)) (println (* 2 (+ 3 1))))
(-main)
