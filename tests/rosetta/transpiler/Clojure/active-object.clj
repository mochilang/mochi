(ns main (:refer-clojure :exclude [sinApprox]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sinApprox)

(def PI 3.141592653589793)

(defn sinApprox [x]
  (try (do (def term x) (def sum x) (def n 1) (while (<= n 12) (do (def denom (double (* (* 2 n) (+ (* 2 n) 1)))) (def term (/ (* (* (- term) x) x) denom)) (def sum (+ sum term)) (def n (+ n 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def dt 0.01)

(def s 0)

(def t1 0)

(def k1 (sinApprox 0))

(def i 1)

(def i2 1)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (<= i 200) (do (def t2 (* (double i) dt)) (def k2 (sinApprox (* t2 PI))) (def s (+ s (* (* (+ k1 k2) 0.5) (- t2 t1)))) (def t1 t2) (def k1 k2) (def i (+ i 1))))
      (while (<= i2 50) (do (def t2 (+ 2 (* (double i2) dt))) (def k2 0) (def s (+ s (* (* (+ k1 k2) 0.5) (- t2 t1)))) (def t1 t2) (def k1 k2) (def i2 (+ i2 1))))
      (println s)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
