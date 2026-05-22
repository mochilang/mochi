(ns main (:refer-clojure :exclude [add]))

(require 'clojure.set)

(defn add [a b]
  (+ a b))

(def add5 (partial add 5))

(defn -main []
  (println (add5 3)))

(-main)
