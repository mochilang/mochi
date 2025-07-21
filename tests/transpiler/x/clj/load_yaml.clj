(ns main)

(require 'clojure.set)

(def people [{:age 30 :email "alice@example.com" :name "Alice"} {:age 15 :email "bob@example.com" :name "Bob"} {:age 20 :email "charlie@example.com" :name "Charlie"}])

(def adults (for [p people :when (>= (:age p) 18)] {:name (:name p) :email (:email p)}))

(defn -main []
  (doseq [a adults] (println (:name a) (:email a))))

(-main)
