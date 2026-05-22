(ns main (:refer-clojure :exclude [sum3]))

(require 'clojure.set)

(defn sum3 [a b c]
  (+ (+ a b) c))

(defn -main []
  (println (sum3 1 2 3)))

(-main)
