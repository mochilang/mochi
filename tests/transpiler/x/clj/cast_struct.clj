(ns main)

(require 'clojure.set)

(defrecord Anon1 [title])

(def todo {:title "hi"})

(defn -main []
  (println (:title todo)))

(-main)
