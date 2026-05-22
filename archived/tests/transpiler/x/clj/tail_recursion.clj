(ns main (:refer-clojure :exclude [sum_rec]))

(require 'clojure.set)

(defn sum_rec [n acc]
  (if (= n 0) acc (sum_rec (- n 1) (+ acc n))))

(defn -main []
  (println (sum_rec 10 0)))

(-main)
