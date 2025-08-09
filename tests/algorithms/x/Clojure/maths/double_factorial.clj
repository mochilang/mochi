(ns main (:refer-clojure :exclude [double_factorial_recursive double_factorial_iterative test_double_factorial main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare double_factorial_recursive double_factorial_iterative test_double_factorial main)

(def ^:dynamic double_factorial_iterative_i nil)

(def ^:dynamic double_factorial_iterative_result nil)

(def ^:dynamic test_double_factorial_n nil)

(defn double_factorial_recursive [double_factorial_recursive_n]
  (try (do (when (< double_factorial_recursive_n 0) (throw (Exception. "double_factorial_recursive() not defined for negative values"))) (if (<= double_factorial_recursive_n 1) 1 (* double_factorial_recursive_n (double_factorial_recursive (- double_factorial_recursive_n 2))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn double_factorial_iterative [double_factorial_iterative_n]
  (binding [double_factorial_iterative_i nil double_factorial_iterative_result nil] (try (do (when (< double_factorial_iterative_n 0) (throw (Exception. "double_factorial_iterative() not defined for negative values"))) (set! double_factorial_iterative_result 1) (set! double_factorial_iterative_i double_factorial_iterative_n) (while (> double_factorial_iterative_i 0) (do (set! double_factorial_iterative_result (* double_factorial_iterative_result double_factorial_iterative_i)) (set! double_factorial_iterative_i (- double_factorial_iterative_i 2)))) (throw (ex-info "return" {:v double_factorial_iterative_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_double_factorial []
  (binding [test_double_factorial_n nil] (do (when (not= (double_factorial_recursive 0) 1) (throw (Exception. "0!! recursive failed"))) (when (not= (double_factorial_iterative 0) 1) (throw (Exception. "0!! iterative failed"))) (when (not= (double_factorial_recursive 1) 1) (throw (Exception. "1!! recursive failed"))) (when (not= (double_factorial_iterative 1) 1) (throw (Exception. "1!! iterative failed"))) (when (not= (double_factorial_recursive 5) 15) (throw (Exception. "5!! recursive failed"))) (when (not= (double_factorial_iterative 5) 15) (throw (Exception. "5!! iterative failed"))) (when (not= (double_factorial_recursive 6) 48) (throw (Exception. "6!! recursive failed"))) (when (not= (double_factorial_iterative 6) 48) (throw (Exception. "6!! iterative failed"))) (set! test_double_factorial_n 0) (while (<= test_double_factorial_n 10) (do (when (not= (double_factorial_recursive test_double_factorial_n) (double_factorial_iterative test_double_factorial_n)) (throw (Exception. "double factorial mismatch"))) (set! test_double_factorial_n (+ test_double_factorial_n 1)))))))

(defn main []
  (do (test_double_factorial) (println (double_factorial_iterative 10))))

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
