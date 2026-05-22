(ns main)

(require 'clojure.set)

(defn -main []
  (doseq [i (range 1 4)] (println i)))

(-main)
