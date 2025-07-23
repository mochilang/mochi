(ns main)

(require 'clojure.set)

(def doors [])

(defn -main []
  (dotimes [i 100] (def doors (conj doors false)))
  (doseq [pass (range 1 101)] (do (def idx (- pass 1)) (while (< idx 100) (do (def doors (assoc doors idx (not (nth doors idx)))) (def idx (+ idx pass))))))
  (dotimes [row 10] (do (def line "") (dotimes [col 10] (do (def idx (+ (* row 10) col)) (if (nth doors idx) (def line (str line "1")) (def line (str line "0"))) (when (< col 9) (def line (str line " "))))) (println line))))

(-main)
