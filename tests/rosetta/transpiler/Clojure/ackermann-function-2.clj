(ns main (:refer-clojure :exclude [pow ackermann2 main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow ackermann2 main)

(defn pow [base exp]
  (try (do (def result 1) (def i 0) (while (< i exp) (do (def result (* result base)) (def i (+ i 1)))) (throw (ex-info "return" {:v result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ackermann2 [m n]
  (do (when (= m 0) (throw (ex-info "return" {:v (+ n 1)}))) (when (= m 1) (throw (ex-info "return" {:v (+ n 2)}))) (when (= m 2) (throw (ex-info "return" {:v (+ (* 2 n) 3)}))) (when (= m 3) (throw (ex-info "return" {:v (- (* 8 (pow 2 n)) 3)}))) (if (= n 0) (ackermann2 (- m 1) 1) (ackermann2 (- m 1) (ackermann2 m (- n 1))))))

(defn main []
  (do (println (str "A(0, 0) = " (str (ackermann2 0 0)))) (println (str "A(1, 2) = " (str (ackermann2 1 2)))) (println (str "A(2, 4) = " (str (ackermann2 2 4)))) (println (str "A(3, 4) = " (str (ackermann2 3 4))))))

(defn -main []
  (main))

(-main)
