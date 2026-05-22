(ns main)

(require 'clojure.set)

(def testpkg {:Add (fn [a b] (+ a b)) :Pi 3.14 :Answer 42})

(defn -main []
  (println ((:Add testpkg) 2 3))
  (println (:Pi testpkg))
  (println (:Answer testpkg)))

(-main)
