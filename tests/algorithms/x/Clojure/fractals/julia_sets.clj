(ns main (:refer-clojure :exclude [complex_add complex_mul sqrtApprox complex_abs sin_taylor cos_taylor exp_taylor complex_exp eval_quadratic eval_exponential iterate_function prepare_grid julia_demo]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare complex_add complex_mul sqrtApprox complex_abs sin_taylor cos_taylor exp_taylor complex_exp eval_quadratic eval_exponential iterate_function prepare_grid julia_demo)

(def ^:dynamic complex_exp_e nil)

(def ^:dynamic complex_mul_imag nil)

(def ^:dynamic complex_mul_real nil)

(def ^:dynamic cos_taylor_i nil)

(def ^:dynamic cos_taylor_k1 nil)

(def ^:dynamic cos_taylor_k2 nil)

(def ^:dynamic cos_taylor_sum nil)

(def ^:dynamic cos_taylor_term nil)

(def ^:dynamic exp_taylor_i nil)

(def ^:dynamic exp_taylor_sum nil)

(def ^:dynamic exp_taylor_term nil)

(def ^:dynamic iterate_function_i nil)

(def ^:dynamic iterate_function_z_n nil)

(def ^:dynamic julia_demo_c_exp nil)

(def ^:dynamic julia_demo_c_poly nil)

(def ^:dynamic julia_demo_exp_result nil)

(def ^:dynamic julia_demo_grid nil)

(def ^:dynamic julia_demo_poly_result nil)

(def ^:dynamic julia_demo_row_exp nil)

(def ^:dynamic julia_demo_row_poly nil)

(def ^:dynamic julia_demo_x nil)

(def ^:dynamic julia_demo_y nil)

(def ^:dynamic julia_demo_z0 nil)

(def ^:dynamic julia_demo_z_exp nil)

(def ^:dynamic julia_demo_z_poly nil)

(def ^:dynamic prepare_grid_grid nil)

(def ^:dynamic prepare_grid_i nil)

(def ^:dynamic prepare_grid_imag nil)

(def ^:dynamic prepare_grid_j nil)

(def ^:dynamic prepare_grid_real nil)

(def ^:dynamic prepare_grid_row nil)

(def ^:dynamic sin_taylor_i nil)

(def ^:dynamic sin_taylor_k1 nil)

(def ^:dynamic sin_taylor_k2 nil)

(def ^:dynamic sin_taylor_sum nil)

(def ^:dynamic sin_taylor_term nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn complex_add [complex_add_a complex_add_b]
  (try (throw (ex-info "return" {:v {:im (+ (:im complex_add_a) (:im complex_add_b)) :re (+ (:re complex_add_a) (:re complex_add_b))}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn complex_mul [complex_mul_a complex_mul_b]
  (binding [complex_mul_imag nil complex_mul_real nil] (try (do (set! complex_mul_real (- (* (:re complex_mul_a) (:re complex_mul_b)) (* (:im complex_mul_a) (:im complex_mul_b)))) (set! complex_mul_imag (+ (* (:re complex_mul_a) (:im complex_mul_b)) (* (:im complex_mul_a) (:re complex_mul_b)))) (throw (ex-info "return" {:v {:im complex_mul_imag :re complex_mul_real}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn complex_abs [complex_abs_a]
  (try (throw (ex-info "return" {:v (sqrtApprox (+ (* (:re complex_abs_a) (:re complex_abs_a)) (* (:im complex_abs_a) (:im complex_abs_a))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin_taylor [sin_taylor_x]
  (binding [sin_taylor_i nil sin_taylor_k1 nil sin_taylor_k2 nil sin_taylor_sum nil sin_taylor_term nil] (try (do (set! sin_taylor_term sin_taylor_x) (set! sin_taylor_sum sin_taylor_x) (set! sin_taylor_i 1) (while (< sin_taylor_i 10) (do (set! sin_taylor_k1 (* 2.0 (double sin_taylor_i))) (set! sin_taylor_k2 (+ (* 2.0 (double sin_taylor_i)) 1.0)) (set! sin_taylor_term (quot (* (* (- sin_taylor_term) sin_taylor_x) sin_taylor_x) (* sin_taylor_k1 sin_taylor_k2))) (set! sin_taylor_sum (+ sin_taylor_sum sin_taylor_term)) (set! sin_taylor_i (+ sin_taylor_i 1)))) (throw (ex-info "return" {:v sin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cos_taylor [cos_taylor_x]
  (binding [cos_taylor_i nil cos_taylor_k1 nil cos_taylor_k2 nil cos_taylor_sum nil cos_taylor_term nil] (try (do (set! cos_taylor_term 1.0) (set! cos_taylor_sum 1.0) (set! cos_taylor_i 1) (while (< cos_taylor_i 10) (do (set! cos_taylor_k1 (- (* 2.0 (double cos_taylor_i)) 1.0)) (set! cos_taylor_k2 (* 2.0 (double cos_taylor_i))) (set! cos_taylor_term (quot (* (* (- cos_taylor_term) cos_taylor_x) cos_taylor_x) (* cos_taylor_k1 cos_taylor_k2))) (set! cos_taylor_sum (+ cos_taylor_sum cos_taylor_term)) (set! cos_taylor_i (+ cos_taylor_i 1)))) (throw (ex-info "return" {:v cos_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp_taylor [exp_taylor_x]
  (binding [exp_taylor_i nil exp_taylor_sum nil exp_taylor_term nil] (try (do (set! exp_taylor_term 1.0) (set! exp_taylor_sum 1.0) (set! exp_taylor_i 1.0) (while (< exp_taylor_i 20.0) (do (set! exp_taylor_term (quot (* exp_taylor_term exp_taylor_x) exp_taylor_i)) (set! exp_taylor_sum (+ exp_taylor_sum exp_taylor_term)) (set! exp_taylor_i (+ exp_taylor_i 1.0)))) (throw (ex-info "return" {:v exp_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn complex_exp [complex_exp_z]
  (binding [complex_exp_e nil] (try (do (set! complex_exp_e (exp_taylor (:re complex_exp_z))) (throw (ex-info "return" {:v {:im (* complex_exp_e (sin_taylor (:im complex_exp_z))) :re (* complex_exp_e (cos_taylor (:im complex_exp_z)))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn eval_quadratic [eval_quadratic_c eval_quadratic_z]
  (try (throw (ex-info "return" {:v (complex_add (complex_mul eval_quadratic_z eval_quadratic_z) eval_quadratic_c)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn eval_exponential [eval_exponential_c eval_exponential_z]
  (try (throw (ex-info "return" {:v (complex_add (complex_exp eval_exponential_z) eval_exponential_c)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn iterate_function [iterate_function_eval_function iterate_function_c iterate_function_nb_iterations iterate_function_z0 iterate_function_infinity]
  (binding [iterate_function_i nil iterate_function_z_n nil] (try (do (set! iterate_function_z_n iterate_function_z0) (set! iterate_function_i 0) (while (< iterate_function_i iterate_function_nb_iterations) (do (set! iterate_function_z_n (iterate_function_eval_function iterate_function_c iterate_function_z_n)) (when (> (complex_abs iterate_function_z_n) iterate_function_infinity) (throw (ex-info "return" {:v iterate_function_z_n}))) (set! iterate_function_i (+ iterate_function_i 1)))) (throw (ex-info "return" {:v iterate_function_z_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prepare_grid [prepare_grid_window_size prepare_grid_nb_pixels]
  (binding [prepare_grid_grid nil prepare_grid_i nil prepare_grid_imag nil prepare_grid_j nil prepare_grid_real nil prepare_grid_row nil] (try (do (set! prepare_grid_grid []) (set! prepare_grid_i 0) (while (< prepare_grid_i prepare_grid_nb_pixels) (do (set! prepare_grid_row []) (set! prepare_grid_j 0) (while (< prepare_grid_j prepare_grid_nb_pixels) (do (set! prepare_grid_real (+ (- prepare_grid_window_size) (quot (* (* 2.0 prepare_grid_window_size) (double prepare_grid_i)) (double (- prepare_grid_nb_pixels 1))))) (set! prepare_grid_imag (+ (- prepare_grid_window_size) (quot (* (* 2.0 prepare_grid_window_size) (double prepare_grid_j)) (double (- prepare_grid_nb_pixels 1))))) (set! prepare_grid_row (conj prepare_grid_row {:im prepare_grid_imag :re prepare_grid_real})) (set! prepare_grid_j (+ prepare_grid_j 1)))) (set! prepare_grid_grid (conj prepare_grid_grid prepare_grid_row)) (set! prepare_grid_i (+ prepare_grid_i 1)))) (throw (ex-info "return" {:v prepare_grid_grid}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn julia_demo []
  (binding [julia_demo_c_exp nil julia_demo_c_poly nil julia_demo_exp_result nil julia_demo_grid nil julia_demo_poly_result nil julia_demo_row_exp nil julia_demo_row_poly nil julia_demo_x nil julia_demo_y nil julia_demo_z0 nil julia_demo_z_exp nil julia_demo_z_poly nil] (do (set! julia_demo_grid (prepare_grid 1.0 5)) (set! julia_demo_c_poly {:im 0.6 :re (- 0.4)}) (set! julia_demo_c_exp {:im 0.0 :re (- 2.0)}) (set! julia_demo_poly_result []) (set! julia_demo_exp_result []) (set! julia_demo_y 0) (while (< julia_demo_y (count julia_demo_grid)) (do (set! julia_demo_row_poly []) (set! julia_demo_row_exp []) (set! julia_demo_x 0) (while (< julia_demo_x (count (nth julia_demo_grid julia_demo_y))) (do (set! julia_demo_z0 (nth (nth julia_demo_grid julia_demo_y) julia_demo_x)) (set! julia_demo_z_poly (iterate_function eval_quadratic julia_demo_c_poly 20 julia_demo_z0 4.0)) (set! julia_demo_z_exp (iterate_function eval_exponential julia_demo_c_exp 10 julia_demo_z0 10000000000.0)) (set! julia_demo_row_poly (conj julia_demo_row_poly (if (< (complex_abs julia_demo_z_poly) 2.0) 1 0))) (set! julia_demo_row_exp (conj julia_demo_row_exp (if (< (complex_abs julia_demo_z_exp) 10000.0) 1 0))) (set! julia_demo_x (+ julia_demo_x 1)))) (set! julia_demo_poly_result (conj julia_demo_poly_result julia_demo_row_poly)) (set! julia_demo_exp_result (conj julia_demo_exp_result julia_demo_row_exp)) (set! julia_demo_y (+ julia_demo_y 1)))) (println julia_demo_poly_result) (println julia_demo_exp_result))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (julia_demo)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
