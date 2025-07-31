(ns main)

(require 'clojure.set)

(defn -main []
  (println (double (/ (reduce + 0 [1 2 3]) (count [1 2 3])))))

(-main)
