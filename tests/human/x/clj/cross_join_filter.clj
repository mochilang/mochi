(ns cross-join-filter)

(def nums [1 2 3])
(def letters ["A" "B"])

(def pairs
  (for [n nums
        :when (even? n)
        l letters]
    {:n n :l l}))

(println "--- Even pairs ---")
(doseq [p pairs]
  (println (:n p) (:l p)))
