(ns main (:refer-clojure :exclude [sqrtApprox]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrtApprox)

(defn sqrtApprox [x]
  (try (do (def guess x) (def i 0) (while (< i 20) (do (def guess (/ (+ guess (/ x guess)) 2.0)) (def i (+ i 1)))) (throw (ex-info "return" {:v guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def n 10)
      (def sum 0.0)
      (def x 1)
      (while (<= x n) (do (def sum (+ sum (* (double x) (double x)))) (def x (+ x 1))))
      (def rms (sqrtApprox (/ sum (double n))))
      (println (str rms))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
