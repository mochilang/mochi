(ns cross-join-triple)

(def nums [1 2])
(def letters ["A" "B"])
(def bools [true false])

(def combos
  (for [n nums
        l letters
        b bools]
    {:n n :l l :b b}))

(println "--- Cross Join of three lists ---")
(doseq [c combos]
  (println (:n c) (:l c) (:b c)))
