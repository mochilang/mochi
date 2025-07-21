(ns main)

(require 'clojure.set)

(defrecord Todo1 [title])

(def todo {:title "hi"})

(defn -main []
  (println (:title todo)))

(-main)
