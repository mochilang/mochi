(ns main (:refer-clojure :exclude [inc]))

(require 'clojure.set)

(defn inc [c]
  (def c (assoc c :n (+ (:n c) 1))))

(def c {:n 0})

(defn -main []
  (inc c)
  (println (:n c)))

(-main)
