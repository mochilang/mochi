(ns main (:refer-clojure :exclude [inc]))

(require 'clojure.set)

(def k 2)

(defn inc [x]
  (+ x k))

(defn -main []
  (println (inc 3)))

(-main)
