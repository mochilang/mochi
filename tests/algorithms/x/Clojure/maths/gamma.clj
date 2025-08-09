(ns main (:refer-clojure :exclude [absf sqrt ln exp_series powf integrand gamma_iterative gamma_recursive main]))

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

(declare absf sqrt ln exp_series powf integrand gamma_iterative gamma_recursive main)

(def ^:dynamic exp_series_n nil)

(def ^:dynamic exp_series_sum nil)

(def ^:dynamic exp_series_term nil)

(def ^:dynamic gamma_iterative_limit nil)

(def ^:dynamic gamma_iterative_step nil)

(def ^:dynamic gamma_iterative_total nil)

(def ^:dynamic gamma_iterative_x nil)

(def ^:dynamic gamma_recursive_frac nil)

(def ^:dynamic gamma_recursive_int_part nil)

(def ^:dynamic ln_denom nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic ln_y2 nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (< sqrt_x 0.0) (throw (Exception. "sqrt domain error"))) (set! sqrt_guess (/ sqrt_x 2.0)) (set! sqrt_i 0) (while (< sqrt_i 20) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_denom nil ln_k nil ln_sum nil ln_term nil ln_y nil ln_y2 nil] (try (do (when (<= ln_x 0.0) (throw (Exception. "ln domain error"))) (set! ln_y (quot (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_y2 (* ln_y ln_y)) (set! ln_term ln_y) (set! ln_sum 0.0) (set! ln_k 0) (while (< ln_k 10) (do (set! ln_denom (double (+ (* 2 ln_k) 1))) (set! ln_sum (+ ln_sum (quot ln_term ln_denom))) (set! ln_term (* ln_term ln_y2)) (set! ln_k (+ ln_k 1)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp_series [exp_series_x]
  (binding [exp_series_n nil exp_series_sum nil exp_series_term nil] (try (do (set! exp_series_term 1.0) (set! exp_series_sum 1.0) (set! exp_series_n 1) (while (< exp_series_n 20) (do (set! exp_series_term (quot (* exp_series_term exp_series_x) (double exp_series_n))) (set! exp_series_sum (+ exp_series_sum exp_series_term)) (set! exp_series_n (+ exp_series_n 1)))) (throw (ex-info "return" {:v exp_series_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn powf [powf_base powf_exponent]
  (try (if (<= powf_base 0.0) 0.0 (exp_series (* powf_exponent (ln powf_base)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn integrand [integrand_x integrand_z]
  (try (throw (ex-info "return" {:v (* (powf integrand_x (- integrand_z 1.0)) (exp_series (- integrand_x)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gamma_iterative [gamma_iterative_num]
  (binding [gamma_iterative_limit nil gamma_iterative_step nil gamma_iterative_total nil gamma_iterative_x nil] (try (do (when (<= gamma_iterative_num 0.0) (throw (Exception. "math domain error"))) (set! gamma_iterative_step 0.001) (set! gamma_iterative_limit 100.0) (set! gamma_iterative_x gamma_iterative_step) (set! gamma_iterative_total 0.0) (while (< gamma_iterative_x gamma_iterative_limit) (do (set! gamma_iterative_total (+ gamma_iterative_total (* (integrand gamma_iterative_x gamma_iterative_num) gamma_iterative_step))) (set! gamma_iterative_x (+ gamma_iterative_x gamma_iterative_step)))) (throw (ex-info "return" {:v gamma_iterative_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gamma_recursive [gamma_recursive_num]
  (binding [gamma_recursive_frac nil gamma_recursive_int_part nil] (try (do (when (<= gamma_recursive_num 0.0) (throw (Exception. "math domain error"))) (when (> gamma_recursive_num 171.5) (throw (Exception. "math range error"))) (set! gamma_recursive_int_part (int gamma_recursive_num)) (set! gamma_recursive_frac (- gamma_recursive_num (double gamma_recursive_int_part))) (when (not (or (< (absf gamma_recursive_frac) 0.000001) (< (absf (- gamma_recursive_frac 0.5)) 0.000001))) (throw (Exception. "num must be an integer or a half-integer"))) (when (< (absf (- gamma_recursive_num 0.5)) 0.000001) (throw (ex-info "return" {:v (sqrt main_PI)}))) (if (< (absf (- gamma_recursive_num 1.0)) 0.000001) 1.0 (* (- gamma_recursive_num 1.0) (gamma_recursive (- gamma_recursive_num 1.0))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (gamma_iterative 5.0)) (println (gamma_recursive 5.0)) (println (gamma_recursive 0.5))))

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
