(ns main (:refer-clojure :exclude [triple]))

(require 'clojure.set)

(defn triple [x]
  (* x 3))

(defn -main []
  (println (triple (+ 1 2))))

(-main)
