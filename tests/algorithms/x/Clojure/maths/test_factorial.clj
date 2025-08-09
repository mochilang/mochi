(ns main (:refer-clojure :exclude [factorial factorial_recursive test_zero test_positive_integers test_large_number run_tests main]))

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

(declare factorial factorial_recursive test_zero test_positive_integers test_large_number run_tests main)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_value nil)

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_value nil] (try (do (when (< factorial_n 0) (throw (Exception. "factorial() not defined for negative values"))) (set! factorial_value 1) (set! factorial_i 1) (while (<= factorial_i factorial_n) (do (set! factorial_value (* factorial_value factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial_recursive [factorial_recursive_n]
  (try (do (when (< factorial_recursive_n 0) (throw (Exception. "factorial() not defined for negative values"))) (if (<= factorial_recursive_n 1) 1 (* factorial_recursive_n (factorial_recursive (- factorial_recursive_n 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_zero []
  (do (when (not= (factorial 0) 1) (throw (Exception. "factorial(0) failed"))) (when (not= (factorial_recursive 0) 1) (throw (Exception. "factorial_recursive(0) failed")))))

(defn test_positive_integers []
  (do (when (not= (factorial 1) 1) (throw (Exception. "factorial(1) failed"))) (when (not= (factorial_recursive 1) 1) (throw (Exception. "factorial_recursive(1) failed"))) (when (not= (factorial 5) 120) (throw (Exception. "factorial(5) failed"))) (when (not= (factorial_recursive 5) 120) (throw (Exception. "factorial_recursive(5) failed"))) (when (not= (factorial 7) 5040) (throw (Exception. "factorial(7) failed"))) (when (not= (factorial_recursive 7) 5040) (throw (Exception. "factorial_recursive(7) failed")))))

(defn test_large_number []
  (do (when (not= (factorial 10) 3628800) (throw (Exception. "factorial(10) failed"))) (when (not= (factorial_recursive 10) 3628800) (throw (Exception. "factorial_recursive(10) failed")))))

(defn run_tests []
  (do (test_zero) (test_positive_integers) (test_large_number)))

(defn main []
  (do (run_tests) (println (factorial 6))))

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
