(ns main)

(require 'clojure.set)

(def s "mochi")

(defn -main []
  (println (nth s 1)))

(-main)
