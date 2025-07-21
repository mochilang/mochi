(ns main (:refer-clojure :exclude [outer]))

(require 'clojure.set)

(defn inner [x y]
  (+ x y))

(defn outer [x]
  (inner x 5))

(defn -main []
  (println (outer 3)))

(-main)
