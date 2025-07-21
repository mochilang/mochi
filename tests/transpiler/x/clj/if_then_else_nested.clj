(ns main)

(require 'clojure.set)

(def x 8)

(def msg (if (> x 10) "big" (if (> x 5) "medium" "small")))

(defn -main []
  (println msg))

(-main)
