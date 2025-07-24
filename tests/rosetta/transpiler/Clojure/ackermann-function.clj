(ns main (:refer-clojure :exclude [ackermann main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn ackermann [m n]
  (do (when (= m 0) (throw (ex-info "return" {:v (+ n 1)}))) (if (= n 0) (ackermann (- m 1) 1) (ackermann (- m 1) (ackermann m (- n 1))))))

(defn main []
  (do (println (str "A(0, 0) = " (str (ackermann 0 0)))) (println (str "A(1, 2) = " (str (ackermann 1 2)))) (println (str "A(2, 4) = " (str (ackermann 2 4)))) (println (str "A(3, 4) = " (str (ackermann 3 4))))))

(defn -main []
  (main))

(-main)
