(ns main)

(require 'clojure.set)

(def result "")

(defn -main []
  (doseq [i (range 1 101)] (do (def j 1) (while (< (* j j) i) (def j (+ j 1))) (if (= (* j j) i) (def result (str result "O")) (def result (str result "-")))))
  (println result))

(-main)
