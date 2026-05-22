(ns main)

(require 'clojure.set)

(def nums [1 2 3])

(def letters ["A" "B"])

(def pairs (for [n nums l letters :when (= (mod n 2) 0)] {:n n :l l}))

(defn -main []
  (println "--- Even pairs ---")
  (doseq [p pairs] (println (:n p) (:l p))))

(-main)
