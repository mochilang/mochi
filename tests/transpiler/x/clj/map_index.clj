(ns main)

(require 'clojure.set)

(defrecord Anon1 [a b])

(def m {"a" 1 "b" 2})

(defn -main []
  (println (get m "b")))

(-main)
