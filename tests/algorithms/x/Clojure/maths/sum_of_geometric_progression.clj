(ns main (:refer-clojure :exclude [pow_float sum_of_geometric_progression test_sum main]))

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

(declare pow_float sum_of_geometric_progression test_sum main)

(def ^:dynamic pow_float_exponent nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(def ^:dynamic sum_of_geometric_progression_a nil)

(def ^:dynamic sum_of_geometric_progression_r nil)

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_exponent nil pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_exponent pow_float_exp) (when (< pow_float_exponent 0) (do (set! pow_float_exponent (- pow_float_exponent)) (set! pow_float_i 0) (while (< pow_float_i pow_float_exponent) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v (/ 1.0 pow_float_result)})))) (set! pow_float_i 0) (while (< pow_float_i pow_float_exponent) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_of_geometric_progression [sum_of_geometric_progression_first_term sum_of_geometric_progression_common_ratio sum_of_geometric_progression_num_of_terms]
  (binding [sum_of_geometric_progression_a nil sum_of_geometric_progression_r nil] (try (do (when (= sum_of_geometric_progression_common_ratio 1) (throw (ex-info "return" {:v (double (* sum_of_geometric_progression_num_of_terms sum_of_geometric_progression_first_term))}))) (set! sum_of_geometric_progression_a (double sum_of_geometric_progression_first_term)) (set! sum_of_geometric_progression_r (double sum_of_geometric_progression_common_ratio)) (throw (ex-info "return" {:v (* (/ sum_of_geometric_progression_a (- 1.0 sum_of_geometric_progression_r)) (- 1.0 (pow_float sum_of_geometric_progression_r sum_of_geometric_progression_num_of_terms)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_sum []
  (do (when (not= (sum_of_geometric_progression 1 2 10) 1023.0) (throw (Exception. "example1 failed"))) (when (not= (sum_of_geometric_progression 1 10 5) 11111.0) (throw (Exception. "example2 failed"))) (when (not= (sum_of_geometric_progression (- 1) 2 10) (- 1023.0)) (throw (Exception. "example3 failed")))))

(defn main []
  (do (test_sum) (println (sum_of_geometric_progression 1 2 10))))

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
