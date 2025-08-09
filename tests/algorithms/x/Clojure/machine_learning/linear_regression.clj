(ns main (:refer-clojure :exclude [dot run_steep_gradient_descent sum_of_square_error run_linear_regression absf mean_absolute_error]))

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

(declare dot run_steep_gradient_descent sum_of_square_error run_linear_regression absf mean_absolute_error)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_sum nil)

(def ^:dynamic main_i nil)

(def ^:dynamic mean_absolute_error_diff nil)

(def ^:dynamic mean_absolute_error_i nil)

(def ^:dynamic mean_absolute_error_total nil)

(def ^:dynamic run_linear_regression_alpha nil)

(def ^:dynamic run_linear_regression_error nil)

(def ^:dynamic run_linear_regression_i nil)

(def ^:dynamic run_linear_regression_iter nil)

(def ^:dynamic run_linear_regression_iterations nil)

(def ^:dynamic run_linear_regression_len_data nil)

(def ^:dynamic run_linear_regression_no_features nil)

(def ^:dynamic run_linear_regression_theta nil)

(def ^:dynamic run_steep_gradient_descent_error nil)

(def ^:dynamic run_steep_gradient_descent_g nil)

(def ^:dynamic run_steep_gradient_descent_gradients nil)

(def ^:dynamic run_steep_gradient_descent_i nil)

(def ^:dynamic run_steep_gradient_descent_j nil)

(def ^:dynamic run_steep_gradient_descent_k nil)

(def ^:dynamic run_steep_gradient_descent_prediction nil)

(def ^:dynamic run_steep_gradient_descent_t nil)

(def ^:dynamic sum_of_square_error_diff nil)

(def ^:dynamic sum_of_square_error_i nil)

(def ^:dynamic sum_of_square_error_prediction nil)

(def ^:dynamic sum_of_square_error_total nil)

(defn dot [dot_x dot_y]
  (binding [dot_i nil dot_sum nil] (try (do (set! dot_sum 0.0) (set! dot_i 0) (while (< dot_i (count dot_x)) (do (set! dot_sum (+ dot_sum (* (nth dot_x dot_i) (nth dot_y dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_steep_gradient_descent [run_steep_gradient_descent_data_x run_steep_gradient_descent_data_y run_steep_gradient_descent_len_data run_steep_gradient_descent_alpha run_steep_gradient_descent_theta]
  (binding [run_steep_gradient_descent_error nil run_steep_gradient_descent_g nil run_steep_gradient_descent_gradients nil run_steep_gradient_descent_i nil run_steep_gradient_descent_j nil run_steep_gradient_descent_k nil run_steep_gradient_descent_prediction nil run_steep_gradient_descent_t nil] (try (do (set! run_steep_gradient_descent_gradients []) (set! run_steep_gradient_descent_j 0) (while (< run_steep_gradient_descent_j (count run_steep_gradient_descent_theta)) (do (set! run_steep_gradient_descent_gradients (conj run_steep_gradient_descent_gradients 0.0)) (set! run_steep_gradient_descent_j (+ run_steep_gradient_descent_j 1)))) (set! run_steep_gradient_descent_i 0) (while (< run_steep_gradient_descent_i run_steep_gradient_descent_len_data) (do (set! run_steep_gradient_descent_prediction (dot run_steep_gradient_descent_theta (nth run_steep_gradient_descent_data_x run_steep_gradient_descent_i))) (set! run_steep_gradient_descent_error (- run_steep_gradient_descent_prediction (nth run_steep_gradient_descent_data_y run_steep_gradient_descent_i))) (set! run_steep_gradient_descent_k 0) (while (< run_steep_gradient_descent_k (count run_steep_gradient_descent_theta)) (do (set! run_steep_gradient_descent_gradients (assoc run_steep_gradient_descent_gradients run_steep_gradient_descent_k (+ (nth run_steep_gradient_descent_gradients run_steep_gradient_descent_k) (* run_steep_gradient_descent_error (nth (nth run_steep_gradient_descent_data_x run_steep_gradient_descent_i) run_steep_gradient_descent_k))))) (set! run_steep_gradient_descent_k (+ run_steep_gradient_descent_k 1)))) (set! run_steep_gradient_descent_i (+ run_steep_gradient_descent_i 1)))) (set! run_steep_gradient_descent_t []) (set! run_steep_gradient_descent_g 0) (while (< run_steep_gradient_descent_g (count run_steep_gradient_descent_theta)) (do (set! run_steep_gradient_descent_t (conj run_steep_gradient_descent_t (- (nth run_steep_gradient_descent_theta run_steep_gradient_descent_g) (* (quot run_steep_gradient_descent_alpha run_steep_gradient_descent_len_data) (nth run_steep_gradient_descent_gradients run_steep_gradient_descent_g))))) (set! run_steep_gradient_descent_g (+ run_steep_gradient_descent_g 1)))) (throw (ex-info "return" {:v run_steep_gradient_descent_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_of_square_error [sum_of_square_error_data_x sum_of_square_error_data_y sum_of_square_error_len_data sum_of_square_error_theta]
  (binding [sum_of_square_error_diff nil sum_of_square_error_i nil sum_of_square_error_prediction nil sum_of_square_error_total nil] (try (do (set! sum_of_square_error_total 0.0) (set! sum_of_square_error_i 0) (while (< sum_of_square_error_i sum_of_square_error_len_data) (do (set! sum_of_square_error_prediction (dot sum_of_square_error_theta (nth sum_of_square_error_data_x sum_of_square_error_i))) (set! sum_of_square_error_diff (- sum_of_square_error_prediction (nth sum_of_square_error_data_y sum_of_square_error_i))) (set! sum_of_square_error_total (+ sum_of_square_error_total (* sum_of_square_error_diff sum_of_square_error_diff))) (set! sum_of_square_error_i (+ sum_of_square_error_i 1)))) (throw (ex-info "return" {:v (quot sum_of_square_error_total (* 2.0 sum_of_square_error_len_data))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_linear_regression [run_linear_regression_data_x run_linear_regression_data_y]
  (binding [run_linear_regression_alpha nil run_linear_regression_error nil run_linear_regression_i nil run_linear_regression_iter nil run_linear_regression_iterations nil run_linear_regression_len_data nil run_linear_regression_no_features nil run_linear_regression_theta nil] (try (do (set! run_linear_regression_iterations 10) (set! run_linear_regression_alpha 0.01) (set! run_linear_regression_no_features (count (nth run_linear_regression_data_x 0))) (set! run_linear_regression_len_data (count run_linear_regression_data_x)) (set! run_linear_regression_theta []) (set! run_linear_regression_i 0) (while (< run_linear_regression_i run_linear_regression_no_features) (do (set! run_linear_regression_theta (conj run_linear_regression_theta 0.0)) (set! run_linear_regression_i (+ run_linear_regression_i 1)))) (set! run_linear_regression_iter 0) (while (< run_linear_regression_iter run_linear_regression_iterations) (do (set! run_linear_regression_theta (run_steep_gradient_descent run_linear_regression_data_x run_linear_regression_data_y run_linear_regression_len_data run_linear_regression_alpha run_linear_regression_theta)) (set! run_linear_regression_error (sum_of_square_error run_linear_regression_data_x run_linear_regression_data_y run_linear_regression_len_data run_linear_regression_theta)) (println (str (str (str "At Iteration " (str (+ run_linear_regression_iter 1))) " - Error is ") (str run_linear_regression_error))) (set! run_linear_regression_iter (+ run_linear_regression_iter 1)))) (throw (ex-info "return" {:v run_linear_regression_theta}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (throw (ex-info "return" {:v (- absf_x)})) (throw (ex-info "return" {:v absf_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mean_absolute_error [mean_absolute_error_predicted_y mean_absolute_error_original_y]
  (binding [mean_absolute_error_diff nil mean_absolute_error_i nil mean_absolute_error_total nil] (try (do (set! mean_absolute_error_total 0.0) (set! mean_absolute_error_i 0) (while (< mean_absolute_error_i (count mean_absolute_error_predicted_y)) (do (set! mean_absolute_error_diff (absf (- (nth mean_absolute_error_predicted_y mean_absolute_error_i) (nth mean_absolute_error_original_y mean_absolute_error_i)))) (set! mean_absolute_error_total (+ mean_absolute_error_total mean_absolute_error_diff)) (set! mean_absolute_error_i (+ mean_absolute_error_i 1)))) (throw (ex-info "return" {:v (quot mean_absolute_error_total (count mean_absolute_error_predicted_y))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data_x [[1.0 1.0] [1.0 2.0] [1.0 3.0]])

(def ^:dynamic main_data_y [1.0 2.0 3.0])

(def ^:dynamic main_theta (run_linear_regression main_data_x main_data_y))

(def ^:dynamic main_i 0)

(def ^:dynamic main_predicted_y [3.0 (- 0.5) 2.0 7.0])

(def ^:dynamic main_original_y [2.5 0.0 2.0 8.0])

(def ^:dynamic main_mae (mean_absolute_error main_predicted_y main_original_y))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Resultant Feature vector :")
      (while (< main_i (count main_theta)) (do (println (str (nth main_theta main_i))) (def main_i (+ main_i 1))))
      (println (str "Mean Absolute Error : " (str main_mae)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
