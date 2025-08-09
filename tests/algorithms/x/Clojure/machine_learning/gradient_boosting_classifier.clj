(ns main (:refer-clojure :exclude [exp_approx signf gradient predict_raw predict train_stump fit accuracy]))

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

(declare exp_approx signf gradient predict_raw predict train_stump fit accuracy)

(def ^:dynamic accuracy_correct nil)

(def ^:dynamic accuracy_i nil)

(def ^:dynamic accuracy_n nil)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic fit_grad nil)

(def ^:dynamic fit_i nil)

(def ^:dynamic fit_m nil)

(def ^:dynamic fit_models nil)

(def ^:dynamic fit_preds nil)

(def ^:dynamic fit_residuals nil)

(def ^:dynamic fit_stump nil)

(def ^:dynamic gradient_exp_val nil)

(def ^:dynamic gradient_i nil)

(def ^:dynamic gradient_n nil)

(def ^:dynamic gradient_res nil)

(def ^:dynamic gradient_residuals nil)

(def ^:dynamic gradient_t nil)

(def ^:dynamic gradient_y nil)

(def ^:dynamic predict_i nil)

(def ^:dynamic predict_raw nil)

(def ^:dynamic predict_raw_i nil)

(def ^:dynamic predict_raw_m nil)

(def ^:dynamic predict_raw_n nil)

(def ^:dynamic predict_raw_preds nil)

(def ^:dynamic predict_raw_stump nil)

(def ^:dynamic predict_raw_value nil)

(def ^:dynamic predict_result nil)

(def ^:dynamic train_stump_best_error nil)

(def ^:dynamic train_stump_best_feature nil)

(def ^:dynamic train_stump_best_left nil)

(def ^:dynamic train_stump_best_right nil)

(def ^:dynamic train_stump_best_threshold nil)

(def ^:dynamic train_stump_count_left nil)

(def ^:dynamic train_stump_count_right nil)

(def ^:dynamic train_stump_diff nil)

(def ^:dynamic train_stump_error nil)

(def ^:dynamic train_stump_i nil)

(def ^:dynamic train_stump_j nil)

(def ^:dynamic train_stump_left_val nil)

(def ^:dynamic train_stump_n_features nil)

(def ^:dynamic train_stump_n_samples nil)

(def ^:dynamic train_stump_pred nil)

(def ^:dynamic train_stump_right_val nil)

(def ^:dynamic train_stump_sum_left nil)

(def ^:dynamic train_stump_sum_right nil)

(def ^:dynamic train_stump_t nil)

(def ^:dynamic train_stump_t_index nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_term 1.0) (set! exp_approx_sum 1.0) (set! exp_approx_i 1) (while (< exp_approx_i 10) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) (double exp_approx_i))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_i (+ exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn signf [signf_x]
  (try (if (>= signf_x 0.0) 1.0 (- 1.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gradient [gradient_target gradient_preds]
  (binding [gradient_exp_val nil gradient_i nil gradient_n nil gradient_res nil gradient_residuals nil gradient_t nil gradient_y nil] (try (do (set! gradient_n (count gradient_target)) (set! gradient_residuals []) (set! gradient_i 0) (while (< gradient_i gradient_n) (do (set! gradient_t (nth gradient_target gradient_i)) (set! gradient_y (nth gradient_preds gradient_i)) (set! gradient_exp_val (exp_approx (* gradient_t gradient_y))) (set! gradient_res (quot (- gradient_t) (+ 1.0 gradient_exp_val))) (set! gradient_residuals (conj gradient_residuals gradient_res)) (set! gradient_i (+ gradient_i 1)))) (throw (ex-info "return" {:v gradient_residuals}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict_raw [predict_raw_models predict_raw_features predict_raw_learning_rate]
  (binding [predict_raw_i nil predict_raw_m nil predict_raw_n nil predict_raw_preds nil predict_raw_stump nil predict_raw_value nil] (try (do (set! predict_raw_n (count predict_raw_features)) (set! predict_raw_preds []) (set! predict_raw_i 0) (while (< predict_raw_i predict_raw_n) (do (set! predict_raw_preds (conj predict_raw_preds 0.0)) (set! predict_raw_i (+ predict_raw_i 1)))) (set! predict_raw_m 0) (while (< predict_raw_m (count predict_raw_models)) (do (set! predict_raw_stump (nth predict_raw_models predict_raw_m)) (set! predict_raw_i 0) (while (< predict_raw_i predict_raw_n) (do (set! predict_raw_value (nth (nth predict_raw_features predict_raw_i) (:feature predict_raw_stump))) (if (<= predict_raw_value (:threshold predict_raw_stump)) (set! predict_raw_preds (assoc predict_raw_preds predict_raw_i (+ (nth predict_raw_preds predict_raw_i) (* predict_raw_learning_rate (:left predict_raw_stump))))) (set! predict_raw_preds (assoc predict_raw_preds predict_raw_i (+ (nth predict_raw_preds predict_raw_i) (* predict_raw_learning_rate (:right predict_raw_stump)))))) (set! predict_raw_i (+ predict_raw_i 1)))) (set! predict_raw_m (+ predict_raw_m 1)))) (throw (ex-info "return" {:v predict_raw_preds}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict [predict_models predict_features predict_learning_rate]
  (binding [predict_i nil predict_raw nil predict_result nil] (try (do (set! predict_raw (predict_raw predict_models predict_features predict_learning_rate)) (set! predict_result []) (set! predict_i 0) (while (< predict_i (count predict_raw)) (do (set! predict_result (conj predict_result (signf (nth predict_raw predict_i)))) (set! predict_i (+ predict_i 1)))) (throw (ex-info "return" {:v predict_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn train_stump [train_stump_features train_stump_residuals]
  (binding [train_stump_best_error nil train_stump_best_feature nil train_stump_best_left nil train_stump_best_right nil train_stump_best_threshold nil train_stump_count_left nil train_stump_count_right nil train_stump_diff nil train_stump_error nil train_stump_i nil train_stump_j nil train_stump_left_val nil train_stump_n_features nil train_stump_n_samples nil train_stump_pred nil train_stump_right_val nil train_stump_sum_left nil train_stump_sum_right nil train_stump_t nil train_stump_t_index nil] (try (do (set! train_stump_n_samples (count train_stump_features)) (set! train_stump_n_features (count (nth train_stump_features 0))) (set! train_stump_best_feature 0) (set! train_stump_best_threshold 0.0) (set! train_stump_best_error 1000000000.0) (set! train_stump_best_left 0.0) (set! train_stump_best_right 0.0) (set! train_stump_j 0) (while (< train_stump_j train_stump_n_features) (do (set! train_stump_t_index 0) (while (< train_stump_t_index train_stump_n_samples) (do (set! train_stump_t (nth (nth train_stump_features train_stump_t_index) train_stump_j)) (set! train_stump_sum_left 0.0) (set! train_stump_count_left 0) (set! train_stump_sum_right 0.0) (set! train_stump_count_right 0) (set! train_stump_i 0) (while (< train_stump_i train_stump_n_samples) (do (if (<= (nth (nth train_stump_features train_stump_i) train_stump_j) train_stump_t) (do (set! train_stump_sum_left (+ train_stump_sum_left (nth train_stump_residuals train_stump_i))) (set! train_stump_count_left (+ train_stump_count_left 1))) (do (set! train_stump_sum_right (+ train_stump_sum_right (nth train_stump_residuals train_stump_i))) (set! train_stump_count_right (+ train_stump_count_right 1)))) (set! train_stump_i (+ train_stump_i 1)))) (set! train_stump_left_val 0.0) (when (not= train_stump_count_left 0) (set! train_stump_left_val (quot train_stump_sum_left (double train_stump_count_left)))) (set! train_stump_right_val 0.0) (when (not= train_stump_count_right 0) (set! train_stump_right_val (quot train_stump_sum_right (double train_stump_count_right)))) (set! train_stump_error 0.0) (set! train_stump_i 0) (while (< train_stump_i train_stump_n_samples) (do (set! train_stump_pred (if (<= (nth (nth train_stump_features train_stump_i) train_stump_j) train_stump_t) train_stump_left_val train_stump_right_val)) (set! train_stump_diff (- (nth train_stump_residuals train_stump_i) train_stump_pred)) (set! train_stump_error (+ train_stump_error (* train_stump_diff train_stump_diff))) (set! train_stump_i (+ train_stump_i 1)))) (when (< train_stump_error train_stump_best_error) (do (set! train_stump_best_error train_stump_error) (set! train_stump_best_feature train_stump_j) (set! train_stump_best_threshold train_stump_t) (set! train_stump_best_left train_stump_left_val) (set! train_stump_best_right train_stump_right_val))) (set! train_stump_t_index (+ train_stump_t_index 1)))) (set! train_stump_j (+ train_stump_j 1)))) (throw (ex-info "return" {:v {:feature train_stump_best_feature :left train_stump_best_left :right train_stump_best_right :threshold train_stump_best_threshold}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fit [fit_n_estimators fit_learning_rate fit_features fit_target]
  (binding [fit_grad nil fit_i nil fit_m nil fit_models nil fit_preds nil fit_residuals nil fit_stump nil] (try (do (set! fit_models []) (set! fit_m 0) (while (< fit_m fit_n_estimators) (do (set! fit_preds (predict_raw fit_models fit_features fit_learning_rate)) (set! fit_grad (gradient fit_target fit_preds)) (set! fit_residuals []) (set! fit_i 0) (while (< fit_i (count fit_grad)) (do (set! fit_residuals (conj fit_residuals (- (nth fit_grad fit_i)))) (set! fit_i (+ fit_i 1)))) (set! fit_stump (train_stump fit_features fit_residuals)) (set! fit_models (conj fit_models fit_stump)) (set! fit_m (+ fit_m 1)))) (throw (ex-info "return" {:v fit_models}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn accuracy [accuracy_preds accuracy_target]
  (binding [accuracy_correct nil accuracy_i nil accuracy_n nil] (try (do (set! accuracy_n (count accuracy_target)) (set! accuracy_correct 0) (set! accuracy_i 0) (while (< accuracy_i accuracy_n) (do (when (= (nth accuracy_preds accuracy_i) (nth accuracy_target accuracy_i)) (set! accuracy_correct (+ accuracy_correct 1))) (set! accuracy_i (+ accuracy_i 1)))) (throw (ex-info "return" {:v (quot (double accuracy_correct) (double accuracy_n))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_features [[1.0] [2.0] [3.0] [4.0]])

(def ^:dynamic main_target [(- 1.0) (- 1.0) 1.0 1.0])

(def ^:dynamic main_models (fit 5 0.5 main_features main_target))

(def ^:dynamic main_predictions (predict main_models main_features 0.5))

(def ^:dynamic main_acc (accuracy main_predictions main_target))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Accuracy: " (str main_acc)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
