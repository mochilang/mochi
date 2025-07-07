(defn boom [a b]
  (println "boom")
  true)

(println (and false (boom 1 2)))
(println (or true (boom 1 2)))
