(ns main)

(require 'clojure.set)

(def i 0)

(defn -main []
  (while (< i 3) (do (println i) (def i (+ i 1)))))

(-main)
