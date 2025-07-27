(ns main (:refer-clojure :exclude [sinApprox cosApprox sqrtApprox]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sinApprox cosApprox sqrtApprox)

(def PI 3.141592653589793)

(defn sinApprox [x]
  (try (do (def term x) (def sum x) (def n 1) (while (<= n 10) (do (def denom (double (* (* 2 n) (+ (* 2 n) 1)))) (def term (/ (* (* (- term) x) x) denom)) (def sum (+ sum term)) (def n (+ n 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cosApprox [x]
  (try (do (def term 1) (def sum 1) (def n 1) (while (<= n 10) (do (def denom (double (* (- (* 2 n) 1) (* 2 n)))) (def term (/ (* (* (- term) x) x) denom)) (def sum (+ sum term)) (def n (+ n 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [x]
  (try (do (def guess x) (def i 0) (while (< i 10) (do (def guess (/ (+ guess (/ x guess)) 2)) (def i (+ i 1)))) (throw (ex-info "return" {:v guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def L 10)

(def G 9.81)

(def dt 0.2)

(def phi0 (/ PI 4))

(def omega (sqrtApprox (/ G L)))

(def t 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (dotimes [step 10] (do (def phi (* phi0 (cosApprox (* omega t)))) (def pos (int (+ (* 10 (sinApprox phi)) 0.5))) (println (str pos)) (def t (+ t dt))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
