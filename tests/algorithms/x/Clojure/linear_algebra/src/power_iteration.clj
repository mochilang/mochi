(ns main (:refer-clojure :exclude [abs sqrtApprox dot mat_vec_mult norm normalize power_iteration]))

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

(declare abs sqrtApprox dot mat_vec_mult norm normalize power_iteration)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_sum nil)

(def ^:dynamic mat_vec_mult_i nil)

(def ^:dynamic mat_vec_mult_res nil)

(def ^:dynamic norm_i nil)

(def ^:dynamic norm_root nil)

(def ^:dynamic norm_sum nil)

(def ^:dynamic normalize_i nil)

(def ^:dynamic normalize_n nil)

(def ^:dynamic normalize_res nil)

(def ^:dynamic power_iteration_denom nil)

(def ^:dynamic power_iteration_err nil)

(def ^:dynamic power_iteration_iterations nil)

(def ^:dynamic power_iteration_lambda nil)

(def ^:dynamic power_iteration_lambda_prev nil)

(def ^:dynamic power_iteration_mv nil)

(def ^:dynamic power_iteration_v nil)

(def ^:dynamic power_iteration_w nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_sum nil] (try (do (set! dot_sum 0.0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_sum (+ dot_sum (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_vec_mult [mat_vec_mult_mat mat_vec_mult_vec]
  (binding [mat_vec_mult_i nil mat_vec_mult_res nil] (try (do (set! mat_vec_mult_res []) (set! mat_vec_mult_i 0) (while (< mat_vec_mult_i (count mat_vec_mult_mat)) (do (set! mat_vec_mult_res (conj mat_vec_mult_res (dot (nth mat_vec_mult_mat mat_vec_mult_i) mat_vec_mult_vec))) (set! mat_vec_mult_i (+ mat_vec_mult_i 1)))) (throw (ex-info "return" {:v mat_vec_mult_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn norm [norm_vec]
  (binding [norm_i nil norm_root nil norm_sum nil] (try (do (set! norm_sum 0.0) (set! norm_i 0) (while (< norm_i (count norm_vec)) (do (set! norm_sum (+ norm_sum (* (nth norm_vec norm_i) (nth norm_vec norm_i)))) (set! norm_i (+ norm_i 1)))) (set! norm_root (sqrtApprox norm_sum)) (throw (ex-info "return" {:v norm_root}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn normalize [normalize_vec]
  (binding [normalize_i nil normalize_n nil normalize_res nil] (try (do (set! normalize_n (norm normalize_vec)) (set! normalize_res []) (set! normalize_i 0) (while (< normalize_i (count normalize_vec)) (do (set! normalize_res (conj normalize_res (quot (nth normalize_vec normalize_i) normalize_n))) (set! normalize_i (+ normalize_i 1)))) (throw (ex-info "return" {:v normalize_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn power_iteration [power_iteration_matrix power_iteration_vector power_iteration_error_tol power_iteration_max_iterations]
  (binding [power_iteration_denom nil power_iteration_err nil power_iteration_iterations nil power_iteration_lambda nil power_iteration_lambda_prev nil power_iteration_mv nil power_iteration_v nil power_iteration_w nil] (try (do (set! power_iteration_v (normalize power_iteration_vector)) (set! power_iteration_lambda_prev 0.0) (set! power_iteration_lambda 0.0) (set! power_iteration_err 1000000000000.0) (set! power_iteration_iterations 0) (while (and (> power_iteration_err power_iteration_error_tol) (< power_iteration_iterations power_iteration_max_iterations)) (do (set! power_iteration_w (mat_vec_mult power_iteration_matrix power_iteration_v)) (set! power_iteration_v (normalize power_iteration_w)) (set! power_iteration_mv (mat_vec_mult power_iteration_matrix power_iteration_v)) (set! power_iteration_lambda (dot power_iteration_v power_iteration_mv)) (set! power_iteration_denom (if (not= power_iteration_lambda 0.0) (abs power_iteration_lambda) 1.0)) (set! power_iteration_err (quot (abs (- power_iteration_lambda power_iteration_lambda_prev)) power_iteration_denom)) (set! power_iteration_lambda_prev power_iteration_lambda) (set! power_iteration_iterations (+ power_iteration_iterations 1)))) (throw (ex-info "return" {:v {:eigenvalue power_iteration_lambda :eigenvector power_iteration_v}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_input_matrix [[41.0 4.0 20.0] [4.0 26.0 30.0] [20.0 30.0 50.0]])

(def ^:dynamic main_vector [41.0 4.0 20.0])

(def ^:dynamic main_result (power_iteration main_input_matrix main_vector 0.000000000001 100))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (:eigenvalue main_result)))
      (println (str (:eigenvector main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
