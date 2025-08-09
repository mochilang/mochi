(ns main (:refer-clojure :exclude [sum_of_harmonic_progression abs_val test_sum_of_harmonic_progression main]))

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

(declare sum_of_harmonic_progression abs_val test_sum_of_harmonic_progression main)

(def ^:dynamic sum_of_harmonic_progression_arithmetic_progression nil)

(def ^:dynamic sum_of_harmonic_progression_i nil)

(def ^:dynamic sum_of_harmonic_progression_j nil)

(def ^:dynamic sum_of_harmonic_progression_term nil)

(def ^:dynamic sum_of_harmonic_progression_total nil)

(def ^:dynamic test_sum_of_harmonic_progression_result1 nil)

(def ^:dynamic test_sum_of_harmonic_progression_result2 nil)

(defn sum_of_harmonic_progression [sum_of_harmonic_progression_first_term sum_of_harmonic_progression_common_difference sum_of_harmonic_progression_number_of_terms]
  (binding [sum_of_harmonic_progression_arithmetic_progression nil sum_of_harmonic_progression_i nil sum_of_harmonic_progression_j nil sum_of_harmonic_progression_term nil sum_of_harmonic_progression_total nil] (try (do (set! sum_of_harmonic_progression_arithmetic_progression [(/ 1.0 sum_of_harmonic_progression_first_term)]) (set! sum_of_harmonic_progression_term (/ 1.0 sum_of_harmonic_progression_first_term)) (set! sum_of_harmonic_progression_i 0) (while (< sum_of_harmonic_progression_i (- sum_of_harmonic_progression_number_of_terms 1)) (do (set! sum_of_harmonic_progression_term (+ sum_of_harmonic_progression_term sum_of_harmonic_progression_common_difference)) (set! sum_of_harmonic_progression_arithmetic_progression (conj sum_of_harmonic_progression_arithmetic_progression sum_of_harmonic_progression_term)) (set! sum_of_harmonic_progression_i (+ sum_of_harmonic_progression_i 1)))) (set! sum_of_harmonic_progression_total 0.0) (set! sum_of_harmonic_progression_j 0) (while (< sum_of_harmonic_progression_j (count sum_of_harmonic_progression_arithmetic_progression)) (do (set! sum_of_harmonic_progression_total (+ sum_of_harmonic_progression_total (/ 1.0 (nth sum_of_harmonic_progression_arithmetic_progression sum_of_harmonic_progression_j)))) (set! sum_of_harmonic_progression_j (+ sum_of_harmonic_progression_j 1)))) (throw (ex-info "return" {:v sum_of_harmonic_progression_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_val [abs_val_num]
  (try (if (< abs_val_num 0.0) (- abs_val_num) abs_val_num) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_sum_of_harmonic_progression []
  (binding [test_sum_of_harmonic_progression_result1 nil test_sum_of_harmonic_progression_result2 nil] (do (set! test_sum_of_harmonic_progression_result1 (sum_of_harmonic_progression 0.5 2.0 2)) (when (> (abs_val (- test_sum_of_harmonic_progression_result1 0.75)) 0.0000001) (throw (Exception. "test1 failed"))) (set! test_sum_of_harmonic_progression_result2 (sum_of_harmonic_progression 0.2 5.0 5)) (when (> (abs_val (- test_sum_of_harmonic_progression_result2 0.45666666666666667)) 0.0000001) (throw (Exception. "test2 failed"))))))

(defn main []
  (do (test_sum_of_harmonic_progression) (println (sum_of_harmonic_progression 0.5 2.0 2))))

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
