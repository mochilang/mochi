(ns main)

(require 'clojure.set)

(def x 12)

(def msg (if (> x 10) "yes" "no"))

(defn -main []
  (println msg))

(-main)
