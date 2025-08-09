(ns main (:refer-clojure :exclude [abs factorial pow_float binomial_distribution]))

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

(declare abs factorial pow_float binomial_distribution)

(def ^:dynamic binomial_distribution_coefficient nil)

(def ^:dynamic binomial_distribution_denominator nil)

(def ^:dynamic binomial_distribution_numerator nil)

(def ^:dynamic binomial_distribution_probability nil)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (when (< factorial_n 0) (throw (Exception. "factorial is undefined for negative numbers"))) (set! factorial_result 1) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_result (* factorial_result factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_i 0) (while (< pow_float_i pow_float_exp) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binomial_distribution [binomial_distribution_successes binomial_distribution_trials binomial_distribution_prob]
  (binding [binomial_distribution_coefficient nil binomial_distribution_denominator nil binomial_distribution_numerator nil binomial_distribution_probability nil] (try (do (when (> binomial_distribution_successes binomial_distribution_trials) (throw (Exception. "successes must be lower or equal to trials"))) (when (or (< binomial_distribution_trials 0) (< binomial_distribution_successes 0)) (throw (Exception. "the function is defined for non-negative integers"))) (when (not (and (< 0.0 binomial_distribution_prob) (< binomial_distribution_prob 1.0))) (throw (Exception. "prob has to be in range of 1 - 0"))) (set! binomial_distribution_probability (* (pow_float binomial_distribution_prob binomial_distribution_successes) (pow_float (- 1.0 binomial_distribution_prob) (- binomial_distribution_trials binomial_distribution_successes)))) (set! binomial_distribution_numerator (double (factorial binomial_distribution_trials))) (set! binomial_distribution_denominator (double (* (factorial binomial_distribution_successes) (factorial (- binomial_distribution_trials binomial_distribution_successes))))) (set! binomial_distribution_coefficient (quot binomial_distribution_numerator binomial_distribution_denominator)) (throw (ex-info "return" {:v (* binomial_distribution_probability binomial_distribution_coefficient)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
