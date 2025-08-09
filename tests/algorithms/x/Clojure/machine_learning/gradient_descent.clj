(ns main (:refer-clojure :exclude [absf hypothesis_value calc_error summation_of_cost_derivative get_cost_derivative allclose run_gradient_descent test_gradient_descent]))

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

(declare absf hypothesis_value calc_error summation_of_cost_derivative get_cost_derivative allclose run_gradient_descent test_gradient_descent)

(def ^:dynamic allclose_diff nil)

(def ^:dynamic allclose_i nil)

(def ^:dynamic allclose_limit nil)

(def ^:dynamic hypothesis_value_i nil)

(def ^:dynamic hypothesis_value_value nil)

(def ^:dynamic main_parameter_vector nil)

(def ^:dynamic run_gradient_descent_absolute_error_limit nil)

(def ^:dynamic run_gradient_descent_deriv nil)

(def ^:dynamic run_gradient_descent_i nil)

(def ^:dynamic run_gradient_descent_j nil)

(def ^:dynamic run_gradient_descent_learning_rate nil)

(def ^:dynamic run_gradient_descent_params nil)

(def ^:dynamic run_gradient_descent_relative_error_limit nil)

(def ^:dynamic run_gradient_descent_temp nil)

(def ^:dynamic summation_of_cost_derivative_dp nil)

(def ^:dynamic summation_of_cost_derivative_e nil)

(def ^:dynamic summation_of_cost_derivative_i nil)

(def ^:dynamic summation_of_cost_derivative_sum nil)

(def ^:dynamic test_gradient_descent_dp nil)

(def ^:dynamic test_gradient_descent_i nil)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hypothesis_value [hypothesis_value_input hypothesis_value_params]
  (binding [hypothesis_value_i nil hypothesis_value_value nil] (try (do (set! hypothesis_value_value (nth hypothesis_value_params 0)) (set! hypothesis_value_i 0) (while (< hypothesis_value_i (count hypothesis_value_input)) (do (set! hypothesis_value_value (+ hypothesis_value_value (* (nth hypothesis_value_input hypothesis_value_i) (nth hypothesis_value_params (+ hypothesis_value_i 1))))) (set! hypothesis_value_i (+ hypothesis_value_i 1)))) (throw (ex-info "return" {:v hypothesis_value_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calc_error [calc_error_dp calc_error_params]
  (try (throw (ex-info "return" {:v (- (hypothesis_value (:x calc_error_dp) calc_error_params) (:y calc_error_dp))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn summation_of_cost_derivative [summation_of_cost_derivative_index summation_of_cost_derivative_params summation_of_cost_derivative_data]
  (binding [summation_of_cost_derivative_dp nil summation_of_cost_derivative_e nil summation_of_cost_derivative_i nil summation_of_cost_derivative_sum nil] (try (do (set! summation_of_cost_derivative_sum 0.0) (set! summation_of_cost_derivative_i 0) (while (< summation_of_cost_derivative_i (count summation_of_cost_derivative_data)) (do (set! summation_of_cost_derivative_dp (nth summation_of_cost_derivative_data summation_of_cost_derivative_i)) (set! summation_of_cost_derivative_e (calc_error summation_of_cost_derivative_dp summation_of_cost_derivative_params)) (if (= summation_of_cost_derivative_index (- 1)) (set! summation_of_cost_derivative_sum (+ summation_of_cost_derivative_sum summation_of_cost_derivative_e)) (set! summation_of_cost_derivative_sum (+ summation_of_cost_derivative_sum (* summation_of_cost_derivative_e (get (:x summation_of_cost_derivative_dp) summation_of_cost_derivative_index))))) (set! summation_of_cost_derivative_i (+ summation_of_cost_derivative_i 1)))) (throw (ex-info "return" {:v summation_of_cost_derivative_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_cost_derivative [get_cost_derivative_index get_cost_derivative_params get_cost_derivative_data]
  (try (throw (ex-info "return" {:v (quot (summation_of_cost_derivative get_cost_derivative_index get_cost_derivative_params get_cost_derivative_data) (double (count get_cost_derivative_data)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn allclose [allclose_a allclose_b allclose_atol allclose_rtol]
  (binding [allclose_diff nil allclose_i nil allclose_limit nil] (try (do (set! allclose_i 0) (while (< allclose_i (count allclose_a)) (do (set! allclose_diff (absf (- (nth allclose_a allclose_i) (nth allclose_b allclose_i)))) (set! allclose_limit (+ allclose_atol (* allclose_rtol (absf (nth allclose_b allclose_i))))) (when (> allclose_diff allclose_limit) (throw (ex-info "return" {:v false}))) (set! allclose_i (+ allclose_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_gradient_descent [run_gradient_descent_train_data run_gradient_descent_initial_params]
  (binding [run_gradient_descent_absolute_error_limit nil run_gradient_descent_deriv nil run_gradient_descent_i nil run_gradient_descent_j nil run_gradient_descent_learning_rate nil run_gradient_descent_params nil run_gradient_descent_relative_error_limit nil run_gradient_descent_temp nil] (try (do (set! run_gradient_descent_learning_rate 0.009) (set! run_gradient_descent_absolute_error_limit 0.000002) (set! run_gradient_descent_relative_error_limit 0.0) (set! run_gradient_descent_j 0) (set! run_gradient_descent_params run_gradient_descent_initial_params) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! run_gradient_descent_j (+ run_gradient_descent_j 1)) (set! run_gradient_descent_temp []) (set! run_gradient_descent_i 0) (while (< run_gradient_descent_i (count run_gradient_descent_params)) (do (set! run_gradient_descent_deriv (get_cost_derivative (- run_gradient_descent_i 1) run_gradient_descent_params run_gradient_descent_train_data)) (set! run_gradient_descent_temp (conj run_gradient_descent_temp (- (nth run_gradient_descent_params run_gradient_descent_i) (* run_gradient_descent_learning_rate run_gradient_descent_deriv)))) (set! run_gradient_descent_i (+ run_gradient_descent_i 1)))) (cond (allclose run_gradient_descent_params run_gradient_descent_temp run_gradient_descent_absolute_error_limit run_gradient_descent_relative_error_limit) (do (println (str "Number of iterations:" (str run_gradient_descent_j))) (recur false)) :else (do (set! run_gradient_descent_params run_gradient_descent_temp) (recur while_flag_1)))))) (throw (ex-info "return" {:v run_gradient_descent_params}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_gradient_descent [test_gradient_descent_test_data test_gradient_descent_params]
  (binding [test_gradient_descent_dp nil test_gradient_descent_i nil] (do (set! test_gradient_descent_i 0) (while (< test_gradient_descent_i (count test_gradient_descent_test_data)) (do (set! test_gradient_descent_dp (nth test_gradient_descent_test_data test_gradient_descent_i)) (println (str "Actual output value:" (str (:y test_gradient_descent_dp)))) (println (str "Hypothesis output:" (str (hypothesis_value (:x test_gradient_descent_dp) test_gradient_descent_params)))) (set! test_gradient_descent_i (+ test_gradient_descent_i 1)))) test_gradient_descent_test_data)))

(def ^:dynamic main_train_data [{:x [5.0 2.0 3.0] :y 15.0} {:x [6.0 5.0 9.0] :y 25.0} {:x [11.0 12.0 13.0] :y 41.0} {:x [1.0 1.0 1.0] :y 8.0} {:x [11.0 12.0 13.0] :y 41.0}])

(def ^:dynamic main_test_data [{:x [515.0 22.0 13.0] :y 555.0} {:x [61.0 35.0 49.0] :y 150.0}])

(def ^:dynamic main_parameter_vector [2.0 4.0 1.0 5.0])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_parameter_vector (run_gradient_descent main_train_data main_parameter_vector))
      (println "\nTesting gradient descent for a linear hypothesis function.\n")
      (test_gradient_descent main_test_data main_parameter_vector)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
