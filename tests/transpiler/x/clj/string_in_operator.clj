(ns main)

(require 'clojure.set)

(def s "catch")

(defn -main []
  (println (clojure.string/includes? s "cat"))
  (println (clojure.string/includes? s "dog")))

(-main)
