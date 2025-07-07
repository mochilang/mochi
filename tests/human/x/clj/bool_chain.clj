(ns bool-chain)

(defn boom []
  (println "boom")
  true)

(println (and (< 1 2) (< 2 3) (< 3 4)))
(println (and (< 1 2) (> 2 3) (boom)))
(println (and (< 1 2) (< 2 3) (> 3 4) (boom)))
