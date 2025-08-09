(ns main (:refer-clojure :exclude [sqrt mean standardize covariance_matrix normalize eigen_decomposition_2x2 transpose matrix_multiply apply_pca]))

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

(declare sqrt mean standardize covariance_matrix normalize eigen_decomposition_2x2 transpose matrix_multiply apply_pca)

(def ^:dynamic apply_pca_components nil)

(def ^:dynamic apply_pca_cov nil)

(def ^:dynamic apply_pca_eig nil)

(def ^:dynamic apply_pca_eigenvalues nil)

(def ^:dynamic apply_pca_eigenvectors nil)

(def ^:dynamic apply_pca_i nil)

(def ^:dynamic apply_pca_ratios nil)

(def ^:dynamic apply_pca_standardized nil)

(def ^:dynamic apply_pca_total nil)

(def ^:dynamic apply_pca_transformed nil)

(def ^:dynamic covariance_matrix_cov nil)

(def ^:dynamic covariance_matrix_i nil)

(def ^:dynamic covariance_matrix_j nil)

(def ^:dynamic covariance_matrix_k nil)

(def ^:dynamic covariance_matrix_n_features nil)

(def ^:dynamic covariance_matrix_n_samples nil)

(def ^:dynamic covariance_matrix_row nil)

(def ^:dynamic covariance_matrix_sum nil)

(def ^:dynamic eigen_decomposition_2x2_a nil)

(def ^:dynamic eigen_decomposition_2x2_b nil)

(def ^:dynamic eigen_decomposition_2x2_c nil)

(def ^:dynamic eigen_decomposition_2x2_diff nil)

(def ^:dynamic eigen_decomposition_2x2_discriminant nil)

(def ^:dynamic eigen_decomposition_2x2_eigenvalues nil)

(def ^:dynamic eigen_decomposition_2x2_eigenvectors nil)

(def ^:dynamic eigen_decomposition_2x2_lambda1 nil)

(def ^:dynamic eigen_decomposition_2x2_lambda2 nil)

(def ^:dynamic eigen_decomposition_2x2_tmp_val nil)

(def ^:dynamic eigen_decomposition_2x2_tmp_vec nil)

(def ^:dynamic eigen_decomposition_2x2_v1 nil)

(def ^:dynamic eigen_decomposition_2x2_v2 nil)

(def ^:dynamic main_idx nil)

(def ^:dynamic matrix_multiply_cols_a nil)

(def ^:dynamic matrix_multiply_cols_b nil)

(def ^:dynamic matrix_multiply_i nil)

(def ^:dynamic matrix_multiply_j nil)

(def ^:dynamic matrix_multiply_k nil)

(def ^:dynamic matrix_multiply_result nil)

(def ^:dynamic matrix_multiply_row nil)

(def ^:dynamic matrix_multiply_rows_a nil)

(def ^:dynamic matrix_multiply_rows_b nil)

(def ^:dynamic matrix_multiply_sum nil)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_sum nil)

(def ^:dynamic normalize_i nil)

(def ^:dynamic normalize_j nil)

(def ^:dynamic normalize_n nil)

(def ^:dynamic normalize_res nil)

(def ^:dynamic normalize_sum nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(def ^:dynamic standardize_c nil)

(def ^:dynamic standardize_column nil)

(def ^:dynamic standardize_diff nil)

(def ^:dynamic standardize_i nil)

(def ^:dynamic standardize_j nil)

(def ^:dynamic standardize_k nil)

(def ^:dynamic standardize_m nil)

(def ^:dynamic standardize_means nil)

(def ^:dynamic standardize_n_features nil)

(def ^:dynamic standardize_n_samples nil)

(def ^:dynamic standardize_r nil)

(def ^:dynamic standardize_row nil)

(def ^:dynamic standardize_standardized nil)

(def ^:dynamic standardize_stds nil)

(def ^:dynamic standardize_variance nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(def ^:dynamic transpose_trans nil)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (set! sqrt_guess (if (> sqrt_x 1.0) (/ sqrt_x 2.0) 1.0)) (set! sqrt_i 0) (while (< sqrt_i 20) (do (set! sqrt_guess (* 0.5 (+ sqrt_guess (quot sqrt_x sqrt_guess)))) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean [mean_xs]
  (binding [mean_i nil mean_sum nil] (try (do (set! mean_sum 0.0) (set! mean_i 0) (while (< mean_i (count mean_xs)) (do (set! mean_sum (+ mean_sum (nth mean_xs mean_i))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v (quot mean_sum (count mean_xs))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn standardize [standardize_data]
  (binding [standardize_c nil standardize_column nil standardize_diff nil standardize_i nil standardize_j nil standardize_k nil standardize_m nil standardize_means nil standardize_n_features nil standardize_n_samples nil standardize_r nil standardize_row nil standardize_standardized nil standardize_stds nil standardize_variance nil] (try (do (set! standardize_n_samples (count standardize_data)) (set! standardize_n_features (count (nth standardize_data 0))) (set! standardize_means []) (set! standardize_stds []) (set! standardize_j 0) (while (< standardize_j standardize_n_features) (do (set! standardize_column []) (set! standardize_i 0) (while (< standardize_i standardize_n_samples) (do (set! standardize_column (conj standardize_column (nth (nth standardize_data standardize_i) standardize_j))) (set! standardize_i (+ standardize_i 1)))) (set! standardize_m (mean standardize_column)) (set! standardize_means (conj standardize_means standardize_m)) (set! standardize_variance 0.0) (set! standardize_k 0) (while (< standardize_k standardize_n_samples) (do (set! standardize_diff (- (nth standardize_column standardize_k) standardize_m)) (set! standardize_variance (+ standardize_variance (* standardize_diff standardize_diff))) (set! standardize_k (+ standardize_k 1)))) (set! standardize_stds (conj standardize_stds (sqrt (quot standardize_variance (- standardize_n_samples 1))))) (set! standardize_j (+ standardize_j 1)))) (set! standardize_standardized []) (set! standardize_r 0) (while (< standardize_r standardize_n_samples) (do (set! standardize_row []) (set! standardize_c 0) (while (< standardize_c standardize_n_features) (do (set! standardize_row (conj standardize_row (quot (- (nth (nth standardize_data standardize_r) standardize_c) (nth standardize_means standardize_c)) (nth standardize_stds standardize_c)))) (set! standardize_c (+ standardize_c 1)))) (set! standardize_standardized (conj standardize_standardized standardize_row)) (set! standardize_r (+ standardize_r 1)))) (throw (ex-info "return" {:v standardize_standardized}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn covariance_matrix [covariance_matrix_data]
  (binding [covariance_matrix_cov nil covariance_matrix_i nil covariance_matrix_j nil covariance_matrix_k nil covariance_matrix_n_features nil covariance_matrix_n_samples nil covariance_matrix_row nil covariance_matrix_sum nil] (try (do (set! covariance_matrix_n_samples (count covariance_matrix_data)) (set! covariance_matrix_n_features (count (nth covariance_matrix_data 0))) (set! covariance_matrix_cov []) (set! covariance_matrix_i 0) (while (< covariance_matrix_i covariance_matrix_n_features) (do (set! covariance_matrix_row []) (set! covariance_matrix_j 0) (while (< covariance_matrix_j covariance_matrix_n_features) (do (set! covariance_matrix_sum 0.0) (set! covariance_matrix_k 0) (while (< covariance_matrix_k covariance_matrix_n_samples) (do (set! covariance_matrix_sum (+ covariance_matrix_sum (* (nth (nth covariance_matrix_data covariance_matrix_k) covariance_matrix_i) (nth (nth covariance_matrix_data covariance_matrix_k) covariance_matrix_j)))) (set! covariance_matrix_k (+ covariance_matrix_k 1)))) (set! covariance_matrix_row (conj covariance_matrix_row (quot covariance_matrix_sum (- covariance_matrix_n_samples 1)))) (set! covariance_matrix_j (+ covariance_matrix_j 1)))) (set! covariance_matrix_cov (conj covariance_matrix_cov covariance_matrix_row)) (set! covariance_matrix_i (+ covariance_matrix_i 1)))) (throw (ex-info "return" {:v covariance_matrix_cov}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn normalize [normalize_vec]
  (binding [normalize_i nil normalize_j nil normalize_n nil normalize_res nil normalize_sum nil] (try (do (set! normalize_sum 0.0) (set! normalize_i 0) (while (< normalize_i (count normalize_vec)) (do (set! normalize_sum (+ normalize_sum (* (nth normalize_vec normalize_i) (nth normalize_vec normalize_i)))) (set! normalize_i (+ normalize_i 1)))) (set! normalize_n (sqrt normalize_sum)) (set! normalize_res []) (set! normalize_j 0) (while (< normalize_j (count normalize_vec)) (do (set! normalize_res (conj normalize_res (quot (nth normalize_vec normalize_j) normalize_n))) (set! normalize_j (+ normalize_j 1)))) (throw (ex-info "return" {:v normalize_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn eigen_decomposition_2x2 [eigen_decomposition_2x2_matrix]
  (binding [eigen_decomposition_2x2_a nil eigen_decomposition_2x2_b nil eigen_decomposition_2x2_c nil eigen_decomposition_2x2_diff nil eigen_decomposition_2x2_discriminant nil eigen_decomposition_2x2_eigenvalues nil eigen_decomposition_2x2_eigenvectors nil eigen_decomposition_2x2_lambda1 nil eigen_decomposition_2x2_lambda2 nil eigen_decomposition_2x2_tmp_val nil eigen_decomposition_2x2_tmp_vec nil eigen_decomposition_2x2_v1 nil eigen_decomposition_2x2_v2 nil] (try (do (set! eigen_decomposition_2x2_a (nth (nth eigen_decomposition_2x2_matrix 0) 0)) (set! eigen_decomposition_2x2_b (nth (nth eigen_decomposition_2x2_matrix 0) 1)) (set! eigen_decomposition_2x2_c (nth (nth eigen_decomposition_2x2_matrix 1) 1)) (set! eigen_decomposition_2x2_diff (- eigen_decomposition_2x2_a eigen_decomposition_2x2_c)) (set! eigen_decomposition_2x2_discriminant (sqrt (+ (* eigen_decomposition_2x2_diff eigen_decomposition_2x2_diff) (* (* 4.0 eigen_decomposition_2x2_b) eigen_decomposition_2x2_b)))) (set! eigen_decomposition_2x2_lambda1 (/ (+ (+ eigen_decomposition_2x2_a eigen_decomposition_2x2_c) eigen_decomposition_2x2_discriminant) 2.0)) (set! eigen_decomposition_2x2_lambda2 (/ (- (+ eigen_decomposition_2x2_a eigen_decomposition_2x2_c) eigen_decomposition_2x2_discriminant) 2.0)) (set! eigen_decomposition_2x2_v1 []) (set! eigen_decomposition_2x2_v2 []) (if (not= eigen_decomposition_2x2_b 0.0) (do (set! eigen_decomposition_2x2_v1 (normalize [(- eigen_decomposition_2x2_lambda1 eigen_decomposition_2x2_c) eigen_decomposition_2x2_b])) (set! eigen_decomposition_2x2_v2 (normalize [(- eigen_decomposition_2x2_lambda2 eigen_decomposition_2x2_c) eigen_decomposition_2x2_b]))) (do (set! eigen_decomposition_2x2_v1 [1.0 0.0]) (set! eigen_decomposition_2x2_v2 [0.0 1.0]))) (set! eigen_decomposition_2x2_eigenvalues [eigen_decomposition_2x2_lambda1 eigen_decomposition_2x2_lambda2]) (set! eigen_decomposition_2x2_eigenvectors [eigen_decomposition_2x2_v1 eigen_decomposition_2x2_v2]) (when (< (nth eigen_decomposition_2x2_eigenvalues 0) (nth eigen_decomposition_2x2_eigenvalues 1)) (do (set! eigen_decomposition_2x2_tmp_val (nth eigen_decomposition_2x2_eigenvalues 0)) (set! eigen_decomposition_2x2_eigenvalues (assoc eigen_decomposition_2x2_eigenvalues 0 (nth eigen_decomposition_2x2_eigenvalues 1))) (set! eigen_decomposition_2x2_eigenvalues (assoc eigen_decomposition_2x2_eigenvalues 1 eigen_decomposition_2x2_tmp_val)) (set! eigen_decomposition_2x2_tmp_vec (nth eigen_decomposition_2x2_eigenvectors 0)) (set! eigen_decomposition_2x2_eigenvectors (assoc eigen_decomposition_2x2_eigenvectors 0 (nth eigen_decomposition_2x2_eigenvectors 1))) (set! eigen_decomposition_2x2_eigenvectors (assoc eigen_decomposition_2x2_eigenvectors 1 eigen_decomposition_2x2_tmp_vec)))) (throw (ex-info "return" {:v {:values eigen_decomposition_2x2_eigenvalues :vectors eigen_decomposition_2x2_eigenvectors}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_matrix]
  (binding [transpose_cols nil transpose_i nil transpose_j nil transpose_row nil transpose_rows nil transpose_trans nil] (try (do (set! transpose_rows (count transpose_matrix)) (set! transpose_cols (count (nth transpose_matrix 0))) (set! transpose_trans []) (set! transpose_i 0) (while (< transpose_i transpose_cols) (do (set! transpose_row []) (set! transpose_j 0) (while (< transpose_j transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_matrix transpose_j) transpose_i))) (set! transpose_j (+ transpose_j 1)))) (set! transpose_trans (conj transpose_trans transpose_row)) (set! transpose_i (+ transpose_i 1)))) (throw (ex-info "return" {:v transpose_trans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_multiply [matrix_multiply_a matrix_multiply_b]
  (binding [matrix_multiply_cols_a nil matrix_multiply_cols_b nil matrix_multiply_i nil matrix_multiply_j nil matrix_multiply_k nil matrix_multiply_result nil matrix_multiply_row nil matrix_multiply_rows_a nil matrix_multiply_rows_b nil matrix_multiply_sum nil] (try (do (set! matrix_multiply_rows_a (count matrix_multiply_a)) (set! matrix_multiply_cols_a (count (nth matrix_multiply_a 0))) (set! matrix_multiply_rows_b (count matrix_multiply_b)) (set! matrix_multiply_cols_b (count (nth matrix_multiply_b 0))) (when (not= matrix_multiply_cols_a matrix_multiply_rows_b) (throw (Exception. "Incompatible matrices"))) (set! matrix_multiply_result []) (set! matrix_multiply_i 0) (while (< matrix_multiply_i matrix_multiply_rows_a) (do (set! matrix_multiply_row []) (set! matrix_multiply_j 0) (while (< matrix_multiply_j matrix_multiply_cols_b) (do (set! matrix_multiply_sum 0.0) (set! matrix_multiply_k 0) (while (< matrix_multiply_k matrix_multiply_cols_a) (do (set! matrix_multiply_sum (+ matrix_multiply_sum (* (nth (nth matrix_multiply_a matrix_multiply_i) matrix_multiply_k) (nth (nth matrix_multiply_b matrix_multiply_k) matrix_multiply_j)))) (set! matrix_multiply_k (+ matrix_multiply_k 1)))) (set! matrix_multiply_row (conj matrix_multiply_row matrix_multiply_sum)) (set! matrix_multiply_j (+ matrix_multiply_j 1)))) (set! matrix_multiply_result (conj matrix_multiply_result matrix_multiply_row)) (set! matrix_multiply_i (+ matrix_multiply_i 1)))) (throw (ex-info "return" {:v matrix_multiply_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn apply_pca [apply_pca_data apply_pca_n_components]
  (binding [apply_pca_components nil apply_pca_cov nil apply_pca_eig nil apply_pca_eigenvalues nil apply_pca_eigenvectors nil apply_pca_i nil apply_pca_ratios nil apply_pca_standardized nil apply_pca_total nil apply_pca_transformed nil] (try (do (set! apply_pca_standardized (standardize apply_pca_data)) (set! apply_pca_cov (covariance_matrix apply_pca_standardized)) (set! apply_pca_eig (eigen_decomposition_2x2 apply_pca_cov)) (set! apply_pca_eigenvalues (:values apply_pca_eig)) (set! apply_pca_eigenvectors (:vectors apply_pca_eig)) (set! apply_pca_components (transpose apply_pca_eigenvectors)) (set! apply_pca_transformed (matrix_multiply apply_pca_standardized apply_pca_components)) (set! apply_pca_total (+ (get apply_pca_eigenvalues 0) (get apply_pca_eigenvalues 1))) (set! apply_pca_ratios []) (set! apply_pca_i 0) (while (< apply_pca_i apply_pca_n_components) (do (set! apply_pca_ratios (conj apply_pca_ratios (quot (get apply_pca_eigenvalues apply_pca_i) apply_pca_total))) (set! apply_pca_i (+ apply_pca_i 1)))) (throw (ex-info "return" {:v {:transformed apply_pca_transformed :variance_ratio apply_pca_ratios}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data [[2.5 2.4] [0.5 0.7] [2.2 2.9] [1.9 2.2] [3.1 3.0] [2.3 2.7] [2.0 1.6] [1.0 1.1] [1.5 1.6] [1.1 0.9]])

(def ^:dynamic main_result (apply_pca main_data 2))

(def ^:dynamic main_idx 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Transformed Data (first 5 rows):")
      (while (< main_idx 5) (do (println (get (:transformed main_result) main_idx)) (def main_idx (+ main_idx 1))))
      (println "Explained Variance Ratio:")
      (println (:variance_ratio main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
