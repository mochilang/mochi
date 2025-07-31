(ns main)

(require 'clojure.set)

(def a (- 10 3))

(def b (+ 2 2))

(defn -main []
  (println a)
  (println (= a 7))
  (println (< b 5)))

(-main)
