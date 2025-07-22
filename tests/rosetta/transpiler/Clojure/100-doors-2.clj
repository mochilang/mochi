(ns main)

(require 'clojure.set)

(def door 1)

(def incrementer 0)

(defn -main []
  (doseq [current (range 1 101)] (do (def line (str (str "Door " (str current)) " ")) (if (= current door) (do (def line (str line "Open")) (def incrementer (+ incrementer 1)) (def door (+ (* (+ door 2) incrementer) 1))) (def line (str line "Closed"))) (println line))))

(-main)
