(ns main (:refer-clojure :exclude [isPrime sumDigits pad main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn isPrime [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v false}))) (when (= (mod n 2) 0) (throw (ex-info "return" {:v (= n 2)}))) (when (= (mod n 3) 0) (throw (ex-info "return" {:v (= n 3)}))) (def d 5) (while (<= (* d d) n) (do (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+ d 2)) (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+ d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sumDigits [n]
  (try (do (def s 0) (def x n) (while (> x 0) (do (def s (+ s (mod x 10))) (def x (int (/ x 10))))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n]
  (do (when (< n 10) (throw (ex-info "return" {:v (str "  " (str n))}))) (if (< n 100) (str " " (str n)) (str n))))

(defn main []
  (do (println "Additive primes less than 500:") (def count 0) (def line "") (def lineCount 0) (def i 2) (while (< i 500) (do (when (and (isPrime i) (isPrime (sumDigits i))) (do (def count (+ count 1)) (def line (str (str line (pad i)) "  ")) (def lineCount (+ lineCount 1)) (when (= lineCount 10) (do (println (subs line 0 (- (count line) 2))) (def line "") (def lineCount 0))))) (if (> i 2) (def i (+ i 2)) (def i (+ i 1))))) (when (> lineCount 0) (println (subs line 0 (- (count line) 2)))) (println (str (str count) " additive primes found."))))

(defn -main []
  (main))

(-main)
