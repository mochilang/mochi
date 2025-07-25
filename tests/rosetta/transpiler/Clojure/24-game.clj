(ns main (:refer-clojure :exclude [randDigit main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare randDigit main)

(defn randDigit []
  (try (throw (ex-info "return" {:v (+ (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 9) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def digits []) (dotimes [i 4] (def digits (conj digits (randDigit)))) (def numstr "") (dotimes [i 4] (def numstr (str numstr (str (nth digits i))))) (println (str (str "Your numbers: " numstr) "\n")) (println "Enter RPN: ") (def expr (read-line)) (when (not= (count expr) 7) (do (println "invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)") (throw (ex-info "return" {:v nil})))) (def stack []) (def i 0) (def valid true) (loop [while_flag_1 true] (when (and while_flag_1 (< i (count expr))) (do (def ch (subs expr i (+ i 1))) (if (and (>= ch "0") (<= ch "9")) (do (when (= (count digits) 0) (do (println "too many numbers.") (throw (ex-info "return" {:v nil})))) (def j 0) (while (not= (nth digits j) (- (int ch) (int "0"))) (do (def j (+ j 1)) (when (= j (count digits)) (do (println "wrong numbers.") (throw (ex-info "return" {:v nil})))))) (def digits (+ (subvec digits 0 j) (subvec digits (+ j 1) (count digits)))) (def stack (conj stack (float (- (int ch) (int "0")))))) (do (when (< (count stack) 2) (do (println "invalid expression syntax.") (def valid false) (recur false))) (def b (nth stack (- (count stack) 1))) (def a (nth stack (- (count stack) 2))) (if (= ch "+") (def stack (assoc stack (- (count stack) 2) (+ a b))) (if (= ch "-") (def stack (assoc stack (- (count stack) 2) (- a b))) (if (= ch "*") (def stack (assoc stack (- (count stack) 2) (* a b))) (if (= ch "/") (def stack (assoc stack (- (count stack) 2) (/ a b))) (do (println (str ch " invalid.")) (def valid false) (recur false)))))) (def stack (subvec stack 0 (- (count stack) 1))))) (def i (+ i 1)) (cond :else (recur while_flag_1))))) (when valid (if (> (Math/abs (- (nth stack 0) 24)) 0.000001) (println (str (str "incorrect. " (str (nth stack 0))) " != 24")) (println "correct.")))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
