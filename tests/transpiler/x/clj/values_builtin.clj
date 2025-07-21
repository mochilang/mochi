(ns main)

(require 'clojure.set)

(defrecord Anon1 [a b c])

(def m {"a" 1 "b" 2 "c" 3})

(defn -main []
  (println (vals m)))

(-main)
