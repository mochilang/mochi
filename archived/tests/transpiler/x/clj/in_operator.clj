(ns main)

(require 'clojure.set)

(def xs [1 2 3])

(defn -main []
  (println (boolean (some #{2} xs)))
  (println (not (boolean (some #{5} xs)))))

(-main)
