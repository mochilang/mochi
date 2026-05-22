(ns main)

(require 'clojure.set)

(def book {:title "Go" :author {:name "Bob" :age 42}})

(defn -main []
  (println (:name (:author book))))

(-main)
