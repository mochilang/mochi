(ns main (:refer-clojure :exclude [main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main)

(defn main []
  (do (def ss {:runtimeFields {}}) (println "Create two fields at runtime: \n") (def i 1) (while (<= i 2) (do (println (str (str "  Field #" (str i)) ":\n")) (println "       Enter name  : ") (def name (read-line)) (println "       Enter value : ") (def value (read-line)) (def fields (:runtimeFields ss)) (def fields (assoc fields name value)) (def ss (assoc ss :runtimeFields fields)) (println "\n") (def i (+ i 1)))) (while true (do (println "Which field do you want to inspect ? ") (def name (read-line)) (if (in name (:runtimeFields ss)) (do (def value (nth (:runtimeFields ss) name)) (println (str (str "Its value is '" value) "'")) (throw (ex-info "return" {:v nil}))) (println "There is no field of that name, try again\n"))))))

(defn -main []
  (main))

(-main)
