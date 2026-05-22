(ns main (:refer-clojure :exclude [add]))

(require 'clojure.set)

(defn add [a b]
  (+ a b))

(defn -main []
  (println (add 2 3)))

(-main)
