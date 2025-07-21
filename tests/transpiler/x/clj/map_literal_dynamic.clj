(ns main)

(require 'clojure.set)

(defrecord M [a b])

(def x 3)

(def y 4)

(def m {"a" x "b" y})

(defn -main []
  (println (get m "a") (get m "b")))

(-main)
