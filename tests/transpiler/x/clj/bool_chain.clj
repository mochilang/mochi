(ns main (:refer-clojure :exclude [boom]))

(require 'clojure.set)

(defn boom []
  (println "boom")
  true)

(defn -main []
  (println (and (and (< 1 2) (< 2 3)) (< 3 4)))
  (println (and (and (< 1 2) (> 2 3)) (boom)))
  (println (and (and (and (< 1 2) (< 2 3)) (> 3 4)) (boom))))

(-main)
