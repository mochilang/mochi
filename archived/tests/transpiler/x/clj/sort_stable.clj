(ns main)

(require 'clojure.set)

(defrecord Items [n v])

(def items [{:n 1 :v "a"} {:n 1 :v "b"} {:n 2 :v "c"}])

(def result (for [i (sort-by (fn [i] (:n i)) items)] (:v i)))

(defn -main []
  (println result))

(-main)
