(ns main (:refer-clojure :exclude [int_to_float abs_float exp_approx floor_int dot transpose matmul matvec identity invert normal_equation linear_regression_prediction sarimax_predictor rbf_kernel support_vector_regressor set_at_float sort_float percentile interquartile_range_checker data_safety_checker main]))

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

(declare int_to_float abs_float exp_approx floor_int dot transpose matmul matvec identity invert normal_equation linear_regression_prediction sarimax_predictor rbf_kernel support_vector_regressor set_at_float sort_float percentile interquartile_range_checker data_safety_checker main)

(def ^:dynamic data_safety_checker_i nil)

(def ^:dynamic data_safety_checker_not_safe nil)

(def ^:dynamic data_safety_checker_safe nil)

(def ^:dynamic data_safety_checker_v nil)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_s nil)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic floor_int_i nil)

(def ^:dynamic identity_i nil)

(def ^:dynamic identity_j nil)

(def ^:dynamic identity_res nil)

(def ^:dynamic identity_row nil)

(def ^:dynamic interquartile_range_checker_iqr nil)

(def ^:dynamic interquartile_range_checker_q1 nil)

(def ^:dynamic interquartile_range_checker_q3 nil)

(def ^:dynamic invert_a nil)

(def ^:dynamic invert_factor nil)

(def ^:dynamic invert_i nil)

(def ^:dynamic invert_inv nil)

(def ^:dynamic invert_j nil)

(def ^:dynamic invert_k nil)

(def ^:dynamic invert_n nil)

(def ^:dynamic invert_pivot nil)

(def ^:dynamic linear_regression_prediction_X nil)

(def ^:dynamic linear_regression_prediction_beta nil)

(def ^:dynamic linear_regression_prediction_i nil)

(def ^:dynamic main_vote nil)

(def ^:dynamic matmul_i nil)

(def ^:dynamic matmul_j nil)

(def ^:dynamic matmul_k nil)

(def ^:dynamic matmul_m nil)

(def ^:dynamic matmul_n nil)

(def ^:dynamic matmul_p nil)

(def ^:dynamic matmul_res nil)

(def ^:dynamic matmul_row nil)

(def ^:dynamic matmul_s nil)

(def ^:dynamic matvec_i nil)

(def ^:dynamic matvec_res nil)

(def ^:dynamic normal_equation_Xt nil)

(def ^:dynamic normal_equation_XtX nil)

(def ^:dynamic normal_equation_XtX_inv nil)

(def ^:dynamic normal_equation_Xty nil)

(def ^:dynamic percentile_frac nil)

(def ^:dynamic percentile_idx nil)

(def ^:dynamic percentile_n nil)

(def ^:dynamic percentile_pos nil)

(def ^:dynamic percentile_sorted nil)

(def ^:dynamic rbf_kernel_diff nil)

(def ^:dynamic rbf_kernel_i nil)

(def ^:dynamic rbf_kernel_sum nil)

(def ^:dynamic sarimax_predictor_X nil)

(def ^:dynamic sarimax_predictor_beta nil)

(def ^:dynamic sarimax_predictor_i nil)

(def ^:dynamic sarimax_predictor_n nil)

(def ^:dynamic sarimax_predictor_y nil)

(def ^:dynamic set_at_float_i nil)

(def ^:dynamic set_at_float_res nil)

(def ^:dynamic sort_float_i nil)

(def ^:dynamic sort_float_j nil)

(def ^:dynamic sort_float_key nil)

(def ^:dynamic sort_float_res nil)

(def ^:dynamic support_vector_regressor_den nil)

(def ^:dynamic support_vector_regressor_gamma nil)

(def ^:dynamic support_vector_regressor_i nil)

(def ^:dynamic support_vector_regressor_num nil)

(def ^:dynamic support_vector_regressor_weights nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_res nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(defn int_to_float [int_to_float_x]
  (try (throw (ex-info "return" {:v (* int_to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn abs_float [abs_float_x]
  (try (if (< abs_float_x 0.0) (- 0.0 abs_float_x) abs_float_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_term 1.0) (set! exp_approx_sum 1.0) (set! exp_approx_i 1) (while (< exp_approx_i 10) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) (int_to_float exp_approx_i))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_i (+ exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor_int [floor_int_x]
  (binding [floor_int_i nil] (try (do (set! floor_int_i 0) (while (<= (int_to_float (+ floor_int_i 1)) floor_int_x) (set! floor_int_i (+ floor_int_i 1))) (throw (ex-info "return" {:v floor_int_i}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_s nil] (try (do (set! dot_s 0.0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_s (+ dot_s (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_m]
  (binding [transpose_cols nil transpose_i nil transpose_j nil transpose_res nil transpose_row nil transpose_rows nil] (try (do (set! transpose_rows (count transpose_m)) (set! transpose_cols (count (nth transpose_m 0))) (set! transpose_res []) (set! transpose_j 0) (while (< transpose_j transpose_cols) (do (set! transpose_row []) (set! transpose_i 0) (while (< transpose_i transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_m transpose_i) transpose_j))) (set! transpose_i (+ transpose_i 1)))) (set! transpose_res (conj transpose_res transpose_row)) (set! transpose_j (+ transpose_j 1)))) (throw (ex-info "return" {:v transpose_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matmul [matmul_a matmul_b]
  (binding [matmul_i nil matmul_j nil matmul_k nil matmul_m nil matmul_n nil matmul_p nil matmul_res nil matmul_row nil matmul_s nil] (try (do (set! matmul_n (count matmul_a)) (set! matmul_m (count (nth matmul_b 0))) (set! matmul_p (count matmul_b)) (set! matmul_res []) (set! matmul_i 0) (while (< matmul_i matmul_n) (do (set! matmul_row []) (set! matmul_j 0) (while (< matmul_j matmul_m) (do (set! matmul_s 0.0) (set! matmul_k 0) (while (< matmul_k matmul_p) (do (set! matmul_s (+ matmul_s (* (nth (nth matmul_a matmul_i) matmul_k) (nth (nth matmul_b matmul_k) matmul_j)))) (set! matmul_k (+ matmul_k 1)))) (set! matmul_row (conj matmul_row matmul_s)) (set! matmul_j (+ matmul_j 1)))) (set! matmul_res (conj matmul_res matmul_row)) (set! matmul_i (+ matmul_i 1)))) (throw (ex-info "return" {:v matmul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matvec [matvec_a matvec_b]
  (binding [matvec_i nil matvec_res nil] (try (do (set! matvec_res []) (set! matvec_i 0) (while (< matvec_i (count matvec_a)) (do (set! matvec_res (conj matvec_res (dot (nth matvec_a matvec_i) matvec_b))) (set! matvec_i (+ matvec_i 1)))) (throw (ex-info "return" {:v matvec_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn identity [identity_n]
  (binding [identity_i nil identity_j nil identity_res nil identity_row nil] (try (do (set! identity_res []) (set! identity_i 0) (while (< identity_i identity_n) (do (set! identity_row []) (set! identity_j 0) (while (< identity_j identity_n) (do (set! identity_row (conj identity_row (if (= identity_i identity_j) 1.0 0.0))) (set! identity_j (+ identity_j 1)))) (set! identity_res (conj identity_res identity_row)) (set! identity_i (+ identity_i 1)))) (throw (ex-info "return" {:v identity_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn invert [invert_mat]
  (binding [invert_a nil invert_factor nil invert_i nil invert_inv nil invert_j nil invert_k nil invert_n nil invert_pivot nil] (try (do (set! invert_n (count invert_mat)) (set! invert_a invert_mat) (set! invert_inv (identity invert_n)) (set! invert_i 0) (while (< invert_i invert_n) (do (set! invert_pivot (nth (nth invert_a invert_i) invert_i)) (set! invert_j 0) (while (< invert_j invert_n) (do (set! invert_a (assoc-in invert_a [invert_i invert_j] (quot (nth (nth invert_a invert_i) invert_j) invert_pivot))) (set! invert_inv (assoc-in invert_inv [invert_i invert_j] (quot (nth (nth invert_inv invert_i) invert_j) invert_pivot))) (set! invert_j (+ invert_j 1)))) (set! invert_k 0) (while (< invert_k invert_n) (do (when (not= invert_k invert_i) (do (set! invert_factor (nth (nth invert_a invert_k) invert_i)) (set! invert_j 0) (while (< invert_j invert_n) (do (set! invert_a (assoc-in invert_a [invert_k invert_j] (- (nth (nth invert_a invert_k) invert_j) (* invert_factor (nth (nth invert_a invert_i) invert_j))))) (set! invert_inv (assoc-in invert_inv [invert_k invert_j] (- (nth (nth invert_inv invert_k) invert_j) (* invert_factor (nth (nth invert_inv invert_i) invert_j))))) (set! invert_j (+ invert_j 1)))))) (set! invert_k (+ invert_k 1)))) (set! invert_i (+ invert_i 1)))) (throw (ex-info "return" {:v invert_inv}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn normal_equation [normal_equation_X normal_equation_y]
  (binding [normal_equation_Xt nil normal_equation_XtX nil normal_equation_XtX_inv nil normal_equation_Xty nil] (try (do (set! normal_equation_Xt (transpose normal_equation_X)) (set! normal_equation_XtX (matmul normal_equation_Xt normal_equation_X)) (set! normal_equation_XtX_inv (invert normal_equation_XtX)) (set! normal_equation_Xty (matvec normal_equation_Xt normal_equation_y)) (throw (ex-info "return" {:v (matvec normal_equation_XtX_inv normal_equation_Xty)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn linear_regression_prediction [linear_regression_prediction_train_dt linear_regression_prediction_train_usr linear_regression_prediction_train_mtch linear_regression_prediction_test_dt linear_regression_prediction_test_mtch]
  (binding [linear_regression_prediction_X nil linear_regression_prediction_beta nil linear_regression_prediction_i nil] (try (do (set! linear_regression_prediction_X []) (set! linear_regression_prediction_i 0) (while (< linear_regression_prediction_i (count linear_regression_prediction_train_dt)) (do (set! linear_regression_prediction_X (conj linear_regression_prediction_X [1.0 (nth linear_regression_prediction_train_dt linear_regression_prediction_i) (nth linear_regression_prediction_train_mtch linear_regression_prediction_i)])) (set! linear_regression_prediction_i (+ linear_regression_prediction_i 1)))) (set! linear_regression_prediction_beta (normal_equation linear_regression_prediction_X linear_regression_prediction_train_usr)) (throw (ex-info "return" {:v (abs_float (+ (+ (nth linear_regression_prediction_beta 0) (* (nth linear_regression_prediction_test_dt 0) (nth linear_regression_prediction_beta 1))) (* (nth linear_regression_prediction_test_mtch 0) (nth linear_regression_prediction_beta 2))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sarimax_predictor [sarimax_predictor_train_user sarimax_predictor_train_match sarimax_predictor_test_match]
  (binding [sarimax_predictor_X nil sarimax_predictor_beta nil sarimax_predictor_i nil sarimax_predictor_n nil sarimax_predictor_y nil] (try (do (set! sarimax_predictor_n (count sarimax_predictor_train_user)) (set! sarimax_predictor_X []) (set! sarimax_predictor_y []) (set! sarimax_predictor_i 1) (while (< sarimax_predictor_i sarimax_predictor_n) (do (set! sarimax_predictor_X (conj sarimax_predictor_X [1.0 (nth sarimax_predictor_train_user (- sarimax_predictor_i 1)) (nth sarimax_predictor_train_match sarimax_predictor_i)])) (set! sarimax_predictor_y (conj sarimax_predictor_y (nth sarimax_predictor_train_user sarimax_predictor_i))) (set! sarimax_predictor_i (+ sarimax_predictor_i 1)))) (set! sarimax_predictor_beta (normal_equation sarimax_predictor_X sarimax_predictor_y)) (throw (ex-info "return" {:v (+ (+ (nth sarimax_predictor_beta 0) (* (nth sarimax_predictor_beta 1) (nth sarimax_predictor_train_user (- sarimax_predictor_n 1)))) (* (nth sarimax_predictor_beta 2) (nth sarimax_predictor_test_match 0)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rbf_kernel [rbf_kernel_a rbf_kernel_b rbf_kernel_gamma]
  (binding [rbf_kernel_diff nil rbf_kernel_i nil rbf_kernel_sum nil] (try (do (set! rbf_kernel_sum 0.0) (set! rbf_kernel_i 0) (while (< rbf_kernel_i (count rbf_kernel_a)) (do (set! rbf_kernel_diff (- (nth rbf_kernel_a rbf_kernel_i) (nth rbf_kernel_b rbf_kernel_i))) (set! rbf_kernel_sum (+ rbf_kernel_sum (* rbf_kernel_diff rbf_kernel_diff))) (set! rbf_kernel_i (+ rbf_kernel_i 1)))) (throw (ex-info "return" {:v (exp_approx (* (- rbf_kernel_gamma) rbf_kernel_sum))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn support_vector_regressor [support_vector_regressor_x_train support_vector_regressor_x_test support_vector_regressor_train_user]
  (binding [support_vector_regressor_den nil support_vector_regressor_gamma nil support_vector_regressor_i nil support_vector_regressor_num nil support_vector_regressor_weights nil] (try (do (set! support_vector_regressor_gamma 0.1) (set! support_vector_regressor_weights []) (set! support_vector_regressor_i 0) (while (< support_vector_regressor_i (count support_vector_regressor_x_train)) (do (set! support_vector_regressor_weights (conj support_vector_regressor_weights (rbf_kernel (nth support_vector_regressor_x_train support_vector_regressor_i) (nth support_vector_regressor_x_test 0) support_vector_regressor_gamma))) (set! support_vector_regressor_i (+ support_vector_regressor_i 1)))) (set! support_vector_regressor_num 0.0) (set! support_vector_regressor_den 0.0) (set! support_vector_regressor_i 0) (while (< support_vector_regressor_i (count support_vector_regressor_train_user)) (do (set! support_vector_regressor_num (+ support_vector_regressor_num (* (nth support_vector_regressor_weights support_vector_regressor_i) (nth support_vector_regressor_train_user support_vector_regressor_i)))) (set! support_vector_regressor_den (+ support_vector_regressor_den (nth support_vector_regressor_weights support_vector_regressor_i))) (set! support_vector_regressor_i (+ support_vector_regressor_i 1)))) (throw (ex-info "return" {:v (quot support_vector_regressor_num support_vector_regressor_den)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_at_float [set_at_float_xs set_at_float_idx set_at_float_value]
  (binding [set_at_float_i nil set_at_float_res nil] (try (do (set! set_at_float_i 0) (set! set_at_float_res []) (while (< set_at_float_i (count set_at_float_xs)) (do (if (= set_at_float_i set_at_float_idx) (set! set_at_float_res (conj set_at_float_res set_at_float_value)) (set! set_at_float_res (conj set_at_float_res (nth set_at_float_xs set_at_float_i)))) (set! set_at_float_i (+ set_at_float_i 1)))) (throw (ex-info "return" {:v set_at_float_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_float [sort_float_xs]
  (binding [sort_float_i nil sort_float_j nil sort_float_key nil sort_float_res nil] (try (do (set! sort_float_res sort_float_xs) (set! sort_float_i 1) (while (< sort_float_i (count sort_float_res)) (do (set! sort_float_key (nth sort_float_res sort_float_i)) (set! sort_float_j (- sort_float_i 1)) (while (and (>= sort_float_j 0) (> (nth sort_float_res sort_float_j) sort_float_key)) (do (set! sort_float_res (set_at_float sort_float_res (+ sort_float_j 1) (nth sort_float_res sort_float_j))) (set! sort_float_j (- sort_float_j 1)))) (set! sort_float_res (set_at_float sort_float_res (+ sort_float_j 1) sort_float_key)) (set! sort_float_i (+ sort_float_i 1)))) (throw (ex-info "return" {:v sort_float_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn percentile [percentile_data percentile_q]
  (binding [percentile_frac nil percentile_idx nil percentile_n nil percentile_pos nil percentile_sorted nil] (try (do (set! percentile_sorted (sort_float percentile_data)) (set! percentile_n (count percentile_sorted)) (set! percentile_pos (* (/ percentile_q 100.0) (int_to_float (- percentile_n 1)))) (set! percentile_idx (floor_int percentile_pos)) (set! percentile_frac (- percentile_pos (int_to_float percentile_idx))) (if (< (+ percentile_idx 1) percentile_n) (+ (* (nth percentile_sorted percentile_idx) (- 1.0 percentile_frac)) (* (nth percentile_sorted (+ percentile_idx 1)) percentile_frac)) (nth percentile_sorted percentile_idx))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn interquartile_range_checker [interquartile_range_checker_train_user]
  (binding [interquartile_range_checker_iqr nil interquartile_range_checker_q1 nil interquartile_range_checker_q3 nil] (try (do (set! interquartile_range_checker_q1 (percentile interquartile_range_checker_train_user 25.0)) (set! interquartile_range_checker_q3 (percentile interquartile_range_checker_train_user 75.0)) (set! interquartile_range_checker_iqr (- interquartile_range_checker_q3 interquartile_range_checker_q1)) (throw (ex-info "return" {:v (- interquartile_range_checker_q1 (* interquartile_range_checker_iqr 0.1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn data_safety_checker [data_safety_checker_list_vote data_safety_checker_actual_result]
  (binding [data_safety_checker_i nil data_safety_checker_not_safe nil data_safety_checker_safe nil data_safety_checker_v nil] (try (do (set! data_safety_checker_safe 0) (set! data_safety_checker_not_safe 0) (set! data_safety_checker_i 0) (while (< data_safety_checker_i (count data_safety_checker_list_vote)) (do (set! data_safety_checker_v (nth data_safety_checker_list_vote data_safety_checker_i)) (if (> data_safety_checker_v data_safety_checker_actual_result) (set! data_safety_checker_safe (+ data_safety_checker_not_safe 1)) (if (<= (abs_float (- (abs_float data_safety_checker_v) (abs_float data_safety_checker_actual_result))) 0.1) (set! data_safety_checker_safe (+ data_safety_checker_safe 1)) (set! data_safety_checker_not_safe (+ data_safety_checker_not_safe 1)))) (set! data_safety_checker_i (+ data_safety_checker_i 1)))) (throw (ex-info "return" {:v (> data_safety_checker_safe data_safety_checker_not_safe)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_vote nil] (do (set! main_vote [(linear_regression_prediction [2.0 3.0 4.0 5.0] [5.0 3.0 4.0 6.0] [3.0 1.0 2.0 4.0] [2.0] [2.0]) (sarimax_predictor [4.0 2.0 6.0 8.0] [3.0 1.0 2.0 4.0] [2.0]) (support_vector_regressor [[5.0 2.0] [1.0 5.0] [6.0 2.0]] [[3.0 2.0]] [2.0 1.0 4.0])]) (println (nth main_vote 0)) (println (nth main_vote 1)) (println (nth main_vote 2)) (println (data_safety_checker main_vote 5.0)))))

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
