(ns main (:refer-clojure :exclude [factorial factorial_recursive test_factorial main]))

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

(declare factorial factorial_recursive test_factorial main)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_value nil)

(def ^:dynamic test_factorial_i nil)

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_value nil] (try (do (when (< factorial_n 0) (throw (Exception. "factorial() not defined for negative values"))) (set! factorial_value 1) (set! factorial_i 1) (while (<= factorial_i factorial_n) (do (set! factorial_value (* factorial_value factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial_recursive [factorial_recursive_n]
  (try (do (when (< factorial_recursive_n 0) (throw (Exception. "factorial() not defined for negative values"))) (if (<= factorial_recursive_n 1) 1 (* factorial_recursive_n (factorial_recursive (- factorial_recursive_n 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_factorial []
  (binding [test_factorial_i nil] (do (set! test_factorial_i 0) (while (<= test_factorial_i 10) (do (when (not= (factorial test_factorial_i) (factorial_recursive test_factorial_i)) (throw (Exception. "mismatch between factorial and factorial_recursive"))) (set! test_factorial_i (+ test_factorial_i 1)))) (when (not= (factorial 6) 720) (throw (Exception. "factorial(6) should be 720"))))))

(defn main []
  (do (test_factorial) (println (factorial 6))))

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
