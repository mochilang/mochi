(ns main)

(require 'clojure.set)

(def nums [1 2 3])

(defn -main []
  (println (boolean (some #{2} nums)))
  (println (boolean (some #{4} nums))))

(-main)
