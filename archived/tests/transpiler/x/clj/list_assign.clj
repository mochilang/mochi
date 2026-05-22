(ns main)

(require 'clojure.set)

(def nums [1 2])

(defn -main []
  (def nums (assoc nums 1 3))
  (println (nth nums 1)))

(-main)
