(ns main (:refer-clojure :exclude [sqrtApprox factorial_float pi]))

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

(declare sqrtApprox factorial_float pi)

(def ^:dynamic factorial_float_i nil)

(def ^:dynamic factorial_float_result nil)

(def ^:dynamic pi_constant_term nil)

(def ^:dynamic pi_exponential_term nil)

(def ^:dynamic pi_fact3k nil)

(def ^:dynamic pi_fact6k nil)

(def ^:dynamic pi_factk nil)

(def ^:dynamic pi_iterations nil)

(def ^:dynamic pi_k nil)

(def ^:dynamic pi_k3 nil)

(def ^:dynamic pi_k6 nil)

(def ^:dynamic pi_linear_term nil)

(def ^:dynamic pi_multinomial nil)

(def ^:dynamic pi_partial_sum nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial_float [factorial_float_n]
  (binding [factorial_float_i nil factorial_float_result nil] (try (do (set! factorial_float_result 1.0) (set! factorial_float_i 2) (while (<= factorial_float_i factorial_float_n) (do (set! factorial_float_result (* factorial_float_result (double factorial_float_i))) (set! factorial_float_i (+ factorial_float_i 1)))) (throw (ex-info "return" {:v factorial_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pi [pi_n]
  (binding [pi_constant_term nil pi_exponential_term nil pi_fact3k nil pi_fact6k nil pi_factk nil pi_iterations nil pi_k nil pi_k3 nil pi_k6 nil pi_linear_term nil pi_multinomial nil pi_partial_sum nil] (try (do (when (< pi_n 1) (throw (Exception. "Undefined for non-natural numbers"))) (set! pi_iterations (quot (+ pi_n 13) 14)) (set! pi_constant_term (* 426880.0 (sqrtApprox 10005.0))) (set! pi_exponential_term 1.0) (set! pi_linear_term 13591409.0) (set! pi_partial_sum pi_linear_term) (set! pi_k 1) (while (< pi_k pi_iterations) (do (set! pi_k6 (* 6 pi_k)) (set! pi_k3 (* 3 pi_k)) (set! pi_fact6k (factorial_float pi_k6)) (set! pi_fact3k (factorial_float pi_k3)) (set! pi_factk (factorial_float pi_k)) (set! pi_multinomial (quot pi_fact6k (* (* (* pi_fact3k pi_factk) pi_factk) pi_factk))) (set! pi_linear_term (+ pi_linear_term 545140134.0)) (set! pi_exponential_term (* pi_exponential_term (- 262537412640768000.0))) (set! pi_partial_sum (+ pi_partial_sum (quot (* pi_multinomial pi_linear_term) pi_exponential_term))) (set! pi_k (+ pi_k 1)))) (throw (ex-info "return" {:v (quot pi_constant_term pi_partial_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_n 50)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str "The first " (str main_n)) " digits of pi is: ") (str (pi main_n))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
