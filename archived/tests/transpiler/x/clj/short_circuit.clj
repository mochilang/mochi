(ns main (:refer-clojure :exclude [boom]))

(require 'clojure.set)

(defn boom [a b]
  (println "boom")
  true)

(defn -main []
  (println (and false (boom 1 2)))
  (println (or true (boom 1 2))))

(-main)
