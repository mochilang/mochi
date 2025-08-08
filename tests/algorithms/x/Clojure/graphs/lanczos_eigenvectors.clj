(ns main (:refer-clojure :exclude [rand random sqrtApprox absf dot vector_scale vector_sub vector_add zeros_matrix column validate_adjacency_list multiply_matrix_vector lanczos_iteration jacobi_eigen matmul sort_eigenpairs find_lanczos_eigenvectors list_to_string matrix_to_string]))

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

(declare rand random sqrtApprox absf dot vector_scale vector_sub vector_add zeros_matrix column validate_adjacency_list multiply_matrix_vector lanczos_iteration jacobi_eigen matmul sort_eigenpairs find_lanczos_eigenvectors list_to_string matrix_to_string)

(def ^:dynamic column_col nil)

(def ^:dynamic column_i nil)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_s nil)

(def ^:dynamic find_lanczos_eigenvectors_eig nil)

(def ^:dynamic find_lanczos_eigenvectors_final_vectors nil)

(def ^:dynamic find_lanczos_eigenvectors_res nil)

(def ^:dynamic find_lanczos_eigenvectors_sorted nil)

(def ^:dynamic jacobi_eigen_a nil)

(def ^:dynamic jacobi_eigen_akp nil)

(def ^:dynamic jacobi_eigen_akq nil)

(def ^:dynamic jacobi_eigen_app nil)

(def ^:dynamic jacobi_eigen_apq nil)

(def ^:dynamic jacobi_eigen_aqq nil)

(def ^:dynamic jacobi_eigen_c nil)

(def ^:dynamic jacobi_eigen_eigenvalues nil)

(def ^:dynamic jacobi_eigen_i nil)

(def ^:dynamic jacobi_eigen_iter nil)

(def ^:dynamic jacobi_eigen_j nil)

(def ^:dynamic jacobi_eigen_k nil)

(def ^:dynamic jacobi_eigen_max nil)

(def ^:dynamic jacobi_eigen_n nil)

(def ^:dynamic jacobi_eigen_p nil)

(def ^:dynamic jacobi_eigen_q nil)

(def ^:dynamic jacobi_eigen_s nil)

(def ^:dynamic jacobi_eigen_t nil)

(def ^:dynamic jacobi_eigen_tau nil)

(def ^:dynamic jacobi_eigen_theta nil)

(def ^:dynamic jacobi_eigen_v nil)

(def ^:dynamic jacobi_eigen_val nil)

(def ^:dynamic jacobi_eigen_vkp nil)

(def ^:dynamic jacobi_eigen_vkq nil)

(def ^:dynamic lanczos_iteration_alpha nil)

(def ^:dynamic lanczos_iteration_beta nil)

(def ^:dynamic lanczos_iteration_i nil)

(def ^:dynamic lanczos_iteration_j nil)

(def ^:dynamic lanczos_iteration_n nil)

(def ^:dynamic lanczos_iteration_p nil)

(def ^:dynamic lanczos_iteration_q nil)

(def ^:dynamic lanczos_iteration_r nil)

(def ^:dynamic lanczos_iteration_ss nil)

(def ^:dynamic lanczos_iteration_ss2 nil)

(def ^:dynamic lanczos_iteration_t nil)

(def ^:dynamic lanczos_iteration_v nil)

(def ^:dynamic lanczos_iteration_vnorm nil)

(def ^:dynamic lanczos_iteration_w nil)

(def ^:dynamic lanczos_iteration_wnorm nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic matmul_cols nil)

(def ^:dynamic matmul_i nil)

(def ^:dynamic matmul_inner nil)

(def ^:dynamic matmul_j nil)

(def ^:dynamic matmul_k nil)

(def ^:dynamic matmul_m nil)

(def ^:dynamic matmul_rows nil)

(def ^:dynamic matmul_s nil)

(def ^:dynamic matrix_to_string_i nil)

(def ^:dynamic matrix_to_string_s nil)

(def ^:dynamic multiply_matrix_vector_i nil)

(def ^:dynamic multiply_matrix_vector_j nil)

(def ^:dynamic multiply_matrix_vector_n nil)

(def ^:dynamic multiply_matrix_vector_nb nil)

(def ^:dynamic multiply_matrix_vector_result nil)

(def ^:dynamic multiply_matrix_vector_sum nil)

(def ^:dynamic sort_eigenpairs_i nil)

(def ^:dynamic sort_eigenpairs_j nil)

(def ^:dynamic sort_eigenpairs_n nil)

(def ^:dynamic sort_eigenpairs_r nil)

(def ^:dynamic sort_eigenpairs_tmp nil)

(def ^:dynamic sort_eigenpairs_tv nil)

(def ^:dynamic sort_eigenpairs_values nil)

(def ^:dynamic sort_eigenpairs_vectors nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic validate_adjacency_list_i nil)

(def ^:dynamic validate_adjacency_list_j nil)

(def ^:dynamic validate_adjacency_list_v nil)

(def ^:dynamic vector_add_i nil)

(def ^:dynamic vector_add_res nil)

(def ^:dynamic vector_scale_i nil)

(def ^:dynamic vector_scale_res nil)

(def ^:dynamic vector_sub_i nil)

(def ^:dynamic vector_sub_res nil)

(def ^:dynamic zeros_matrix_i nil)

(def ^:dynamic zeros_matrix_j nil)

(def ^:dynamic zeros_matrix_m nil)

(def ^:dynamic zeros_matrix_row nil)

(def ^:dynamic main_seed 123456789)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (* 1.0 (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn absf [absf_x]
  (try (throw (ex-info "return" {:v (if (< absf_x 0.0) (- absf_x) absf_x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_s nil] (try (do (set! dot_s 0.0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_s (+ dot_s (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_scale [vector_scale_v vector_scale_s]
  (binding [vector_scale_i nil vector_scale_res nil] (try (do (set! vector_scale_res []) (set! vector_scale_i 0) (while (< vector_scale_i (count vector_scale_v)) (do (set! vector_scale_res (conj vector_scale_res (* (nth vector_scale_v vector_scale_i) vector_scale_s))) (set! vector_scale_i (+ vector_scale_i 1)))) (throw (ex-info "return" {:v vector_scale_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_sub [vector_sub_a vector_sub_b]
  (binding [vector_sub_i nil vector_sub_res nil] (try (do (set! vector_sub_res []) (set! vector_sub_i 0) (while (< vector_sub_i (count vector_sub_a)) (do (set! vector_sub_res (conj vector_sub_res (- (nth vector_sub_a vector_sub_i) (nth vector_sub_b vector_sub_i)))) (set! vector_sub_i (+ vector_sub_i 1)))) (throw (ex-info "return" {:v vector_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_add [vector_add_a vector_add_b]
  (binding [vector_add_i nil vector_add_res nil] (try (do (set! vector_add_res []) (set! vector_add_i 0) (while (< vector_add_i (count vector_add_a)) (do (set! vector_add_res (conj vector_add_res (+ (nth vector_add_a vector_add_i) (nth vector_add_b vector_add_i)))) (set! vector_add_i (+ vector_add_i 1)))) (throw (ex-info "return" {:v vector_add_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zeros_matrix [zeros_matrix_r zeros_matrix_c]
  (binding [zeros_matrix_i nil zeros_matrix_j nil zeros_matrix_m nil zeros_matrix_row nil] (try (do (set! zeros_matrix_m []) (set! zeros_matrix_i 0) (while (< zeros_matrix_i zeros_matrix_r) (do (set! zeros_matrix_row []) (set! zeros_matrix_j 0) (while (< zeros_matrix_j zeros_matrix_c) (do (set! zeros_matrix_row (conj zeros_matrix_row 0.0)) (set! zeros_matrix_j (+ zeros_matrix_j 1)))) (set! zeros_matrix_m (conj zeros_matrix_m zeros_matrix_row)) (set! zeros_matrix_i (+ zeros_matrix_i 1)))) (throw (ex-info "return" {:v zeros_matrix_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn column [column_m column_idx]
  (binding [column_col nil column_i nil] (try (do (set! column_col []) (set! column_i 0) (while (< column_i (count column_m)) (do (set! column_col (conj column_col (nth (nth column_m column_i) column_idx))) (set! column_i (+ column_i 1)))) (throw (ex-info "return" {:v column_col}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn validate_adjacency_list [validate_adjacency_list_graph]
  (binding [validate_adjacency_list_i nil validate_adjacency_list_j nil validate_adjacency_list_v nil] (do (set! validate_adjacency_list_i 0) (while (< validate_adjacency_list_i (count validate_adjacency_list_graph)) (do (set! validate_adjacency_list_j 0) (while (< validate_adjacency_list_j (count (nth validate_adjacency_list_graph validate_adjacency_list_i))) (do (set! validate_adjacency_list_v (nth (nth validate_adjacency_list_graph validate_adjacency_list_i) validate_adjacency_list_j)) (when (or (< validate_adjacency_list_v 0) (>= validate_adjacency_list_v (count validate_adjacency_list_graph))) (throw (Exception. "Invalid neighbor"))) (set! validate_adjacency_list_j (+ validate_adjacency_list_j 1)))) (set! validate_adjacency_list_i (+ validate_adjacency_list_i 1)))))))

(defn multiply_matrix_vector [multiply_matrix_vector_graph multiply_matrix_vector_vector]
  (binding [multiply_matrix_vector_i nil multiply_matrix_vector_j nil multiply_matrix_vector_n nil multiply_matrix_vector_nb nil multiply_matrix_vector_result nil multiply_matrix_vector_sum nil] (try (do (set! multiply_matrix_vector_n (count multiply_matrix_vector_graph)) (when (not= (count multiply_matrix_vector_vector) multiply_matrix_vector_n) (throw (Exception. "Vector length must match number of nodes"))) (set! multiply_matrix_vector_result []) (set! multiply_matrix_vector_i 0) (while (< multiply_matrix_vector_i multiply_matrix_vector_n) (do (set! multiply_matrix_vector_sum 0.0) (set! multiply_matrix_vector_j 0) (while (< multiply_matrix_vector_j (count (nth multiply_matrix_vector_graph multiply_matrix_vector_i))) (do (set! multiply_matrix_vector_nb (nth (nth multiply_matrix_vector_graph multiply_matrix_vector_i) multiply_matrix_vector_j)) (set! multiply_matrix_vector_sum (+ multiply_matrix_vector_sum (nth multiply_matrix_vector_vector multiply_matrix_vector_nb))) (set! multiply_matrix_vector_j (+ multiply_matrix_vector_j 1)))) (set! multiply_matrix_vector_result (conj multiply_matrix_vector_result multiply_matrix_vector_sum)) (set! multiply_matrix_vector_i (+ multiply_matrix_vector_i 1)))) (throw (ex-info "return" {:v multiply_matrix_vector_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lanczos_iteration [lanczos_iteration_graph lanczos_iteration_k]
  (binding [lanczos_iteration_alpha nil lanczos_iteration_beta nil lanczos_iteration_i nil lanczos_iteration_j nil lanczos_iteration_n nil lanczos_iteration_p nil lanczos_iteration_q nil lanczos_iteration_r nil lanczos_iteration_ss nil lanczos_iteration_ss2 nil lanczos_iteration_t nil lanczos_iteration_v nil lanczos_iteration_vnorm nil lanczos_iteration_w nil lanczos_iteration_wnorm nil] (try (do (set! lanczos_iteration_n (count lanczos_iteration_graph)) (when (or (< lanczos_iteration_k 1) (> lanczos_iteration_k lanczos_iteration_n)) (throw (Exception. "invalid number of eigenvectors"))) (set! lanczos_iteration_q (zeros_matrix lanczos_iteration_n lanczos_iteration_k)) (set! lanczos_iteration_t (zeros_matrix lanczos_iteration_k lanczos_iteration_k)) (set! lanczos_iteration_v []) (set! lanczos_iteration_i 0) (while (< lanczos_iteration_i lanczos_iteration_n) (do (set! lanczos_iteration_v (conj lanczos_iteration_v (random))) (set! lanczos_iteration_i (+ lanczos_iteration_i 1)))) (set! lanczos_iteration_ss 0.0) (set! lanczos_iteration_i 0) (while (< lanczos_iteration_i lanczos_iteration_n) (do (set! lanczos_iteration_ss (+ lanczos_iteration_ss (* (nth lanczos_iteration_v lanczos_iteration_i) (nth lanczos_iteration_v lanczos_iteration_i)))) (set! lanczos_iteration_i (+ lanczos_iteration_i 1)))) (set! lanczos_iteration_vnorm (sqrtApprox lanczos_iteration_ss)) (set! lanczos_iteration_i 0) (while (< lanczos_iteration_i lanczos_iteration_n) (do (set! lanczos_iteration_q (assoc-in lanczos_iteration_q [lanczos_iteration_i 0] (quot (nth lanczos_iteration_v lanczos_iteration_i) lanczos_iteration_vnorm))) (set! lanczos_iteration_i (+ lanczos_iteration_i 1)))) (set! lanczos_iteration_beta 0.0) (set! lanczos_iteration_j 0) (while (< lanczos_iteration_j lanczos_iteration_k) (do (set! lanczos_iteration_w (multiply_matrix_vector lanczos_iteration_graph (column lanczos_iteration_q lanczos_iteration_j))) (when (> lanczos_iteration_j 0) (set! lanczos_iteration_w (vector_sub lanczos_iteration_w (vector_scale (column lanczos_iteration_q (- lanczos_iteration_j 1)) lanczos_iteration_beta)))) (set! lanczos_iteration_alpha (dot (column lanczos_iteration_q lanczos_iteration_j) lanczos_iteration_w)) (set! lanczos_iteration_w (vector_sub lanczos_iteration_w (vector_scale (column lanczos_iteration_q lanczos_iteration_j) lanczos_iteration_alpha))) (set! lanczos_iteration_ss2 0.0) (set! lanczos_iteration_p 0) (while (< lanczos_iteration_p lanczos_iteration_n) (do (set! lanczos_iteration_ss2 (+ lanczos_iteration_ss2 (* (nth lanczos_iteration_w lanczos_iteration_p) (nth lanczos_iteration_w lanczos_iteration_p)))) (set! lanczos_iteration_p (+ lanczos_iteration_p 1)))) (set! lanczos_iteration_beta (sqrtApprox lanczos_iteration_ss2)) (set! lanczos_iteration_t (assoc-in lanczos_iteration_t [lanczos_iteration_j lanczos_iteration_j] lanczos_iteration_alpha)) (when (< lanczos_iteration_j (- lanczos_iteration_k 1)) (do (set! lanczos_iteration_t (assoc-in lanczos_iteration_t [lanczos_iteration_j (+ lanczos_iteration_j 1)] lanczos_iteration_beta)) (set! lanczos_iteration_t (assoc-in lanczos_iteration_t [(+ lanczos_iteration_j 1) lanczos_iteration_j] lanczos_iteration_beta)) (when (> lanczos_iteration_beta 0.0000000001) (do (set! lanczos_iteration_wnorm (vector_scale lanczos_iteration_w (/ 1.0 lanczos_iteration_beta))) (set! lanczos_iteration_r 0) (while (< lanczos_iteration_r lanczos_iteration_n) (do (set! lanczos_iteration_q (assoc-in lanczos_iteration_q [lanczos_iteration_r (+ lanczos_iteration_j 1)] (nth lanczos_iteration_wnorm lanczos_iteration_r))) (set! lanczos_iteration_r (+ lanczos_iteration_r 1)))))))) (set! lanczos_iteration_j (+ lanczos_iteration_j 1)))) (throw (ex-info "return" {:v {:q lanczos_iteration_q :t lanczos_iteration_t}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn jacobi_eigen [jacobi_eigen_a_in jacobi_eigen_max_iter]
  (binding [jacobi_eigen_a nil jacobi_eigen_akp nil jacobi_eigen_akq nil jacobi_eigen_app nil jacobi_eigen_apq nil jacobi_eigen_aqq nil jacobi_eigen_c nil jacobi_eigen_eigenvalues nil jacobi_eigen_i nil jacobi_eigen_iter nil jacobi_eigen_j nil jacobi_eigen_k nil jacobi_eigen_max nil jacobi_eigen_n nil jacobi_eigen_p nil jacobi_eigen_q nil jacobi_eigen_s nil jacobi_eigen_t nil jacobi_eigen_tau nil jacobi_eigen_theta nil jacobi_eigen_v nil jacobi_eigen_val nil jacobi_eigen_vkp nil jacobi_eigen_vkq nil] (try (do (set! jacobi_eigen_n (count jacobi_eigen_a_in)) (set! jacobi_eigen_a jacobi_eigen_a_in) (set! jacobi_eigen_v (zeros_matrix jacobi_eigen_n jacobi_eigen_n)) (set! jacobi_eigen_i 0) (while (< jacobi_eigen_i jacobi_eigen_n) (do (set! jacobi_eigen_v (assoc-in jacobi_eigen_v [jacobi_eigen_i jacobi_eigen_i] 1.0)) (set! jacobi_eigen_i (+ jacobi_eigen_i 1)))) (set! jacobi_eigen_iter 0) (loop [while_flag_1 true] (when (and while_flag_1 (< jacobi_eigen_iter jacobi_eigen_max_iter)) (do (set! jacobi_eigen_p 0) (set! jacobi_eigen_q 1) (set! jacobi_eigen_max (absf (nth (nth jacobi_eigen_a jacobi_eigen_p) jacobi_eigen_q))) (set! jacobi_eigen_i 0) (while (< jacobi_eigen_i jacobi_eigen_n) (do (set! jacobi_eigen_j (+ jacobi_eigen_i 1)) (while (< jacobi_eigen_j jacobi_eigen_n) (do (set! jacobi_eigen_val (absf (nth (nth jacobi_eigen_a jacobi_eigen_i) jacobi_eigen_j))) (when (> jacobi_eigen_val jacobi_eigen_max) (do (set! jacobi_eigen_max jacobi_eigen_val) (set! jacobi_eigen_p jacobi_eigen_i) (set! jacobi_eigen_q jacobi_eigen_j))) (set! jacobi_eigen_j (+ jacobi_eigen_j 1)))) (set! jacobi_eigen_i (+ jacobi_eigen_i 1)))) (cond (< jacobi_eigen_max 0.00000001) (recur false) :else (do (set! jacobi_eigen_app (nth (nth jacobi_eigen_a jacobi_eigen_p) jacobi_eigen_p)) (set! jacobi_eigen_aqq (nth (nth jacobi_eigen_a jacobi_eigen_q) jacobi_eigen_q)) (set! jacobi_eigen_apq (nth (nth jacobi_eigen_a jacobi_eigen_p) jacobi_eigen_q)) (set! jacobi_eigen_theta (quot (- jacobi_eigen_aqq jacobi_eigen_app) (* 2.0 jacobi_eigen_apq))) (set! jacobi_eigen_t (/ 1.0 (+ (absf jacobi_eigen_theta) (sqrtApprox (+ (* jacobi_eigen_theta jacobi_eigen_theta) 1.0))))) (when (< jacobi_eigen_theta 0.0) (set! jacobi_eigen_t (- jacobi_eigen_t))) (set! jacobi_eigen_c (/ 1.0 (sqrtApprox (+ 1.0 (* jacobi_eigen_t jacobi_eigen_t))))) (set! jacobi_eigen_s (* jacobi_eigen_t jacobi_eigen_c)) (set! jacobi_eigen_tau (quot jacobi_eigen_s (+ 1.0 jacobi_eigen_c))) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_p jacobi_eigen_p] (- jacobi_eigen_app (* jacobi_eigen_t jacobi_eigen_apq)))) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_q jacobi_eigen_q] (+ jacobi_eigen_aqq (* jacobi_eigen_t jacobi_eigen_apq)))) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_p jacobi_eigen_q] 0.0)) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_q jacobi_eigen_p] 0.0)) (set! jacobi_eigen_k 0) (while (< jacobi_eigen_k jacobi_eigen_n) (do (when (and (not= jacobi_eigen_k jacobi_eigen_p) (not= jacobi_eigen_k jacobi_eigen_q)) (do (set! jacobi_eigen_akp (nth (nth jacobi_eigen_a jacobi_eigen_k) jacobi_eigen_p)) (set! jacobi_eigen_akq (nth (nth jacobi_eigen_a jacobi_eigen_k) jacobi_eigen_q)) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_k jacobi_eigen_p] (- jacobi_eigen_akp (* jacobi_eigen_s (+ jacobi_eigen_akq (* jacobi_eigen_tau jacobi_eigen_akp)))))) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_p jacobi_eigen_k] (nth (nth jacobi_eigen_a jacobi_eigen_k) jacobi_eigen_p))) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_k jacobi_eigen_q] (+ jacobi_eigen_akq (* jacobi_eigen_s (- jacobi_eigen_akp (* jacobi_eigen_tau jacobi_eigen_akq)))))) (set! jacobi_eigen_a (assoc-in jacobi_eigen_a [jacobi_eigen_q jacobi_eigen_k] (nth (nth jacobi_eigen_a jacobi_eigen_k) jacobi_eigen_q))))) (set! jacobi_eigen_k (+ jacobi_eigen_k 1)))) (set! jacobi_eigen_k 0) (while (< jacobi_eigen_k jacobi_eigen_n) (do (set! jacobi_eigen_vkp (nth (nth jacobi_eigen_v jacobi_eigen_k) jacobi_eigen_p)) (set! jacobi_eigen_vkq (nth (nth jacobi_eigen_v jacobi_eigen_k) jacobi_eigen_q)) (set! jacobi_eigen_v (assoc-in jacobi_eigen_v [jacobi_eigen_k jacobi_eigen_p] (- jacobi_eigen_vkp (* jacobi_eigen_s (+ jacobi_eigen_vkq (* jacobi_eigen_tau jacobi_eigen_vkp)))))) (set! jacobi_eigen_v (assoc-in jacobi_eigen_v [jacobi_eigen_k jacobi_eigen_q] (+ jacobi_eigen_vkq (* jacobi_eigen_s (- jacobi_eigen_vkp (* jacobi_eigen_tau jacobi_eigen_vkq)))))) (set! jacobi_eigen_k (+ jacobi_eigen_k 1)))) (set! jacobi_eigen_iter (+ jacobi_eigen_iter 1)) (recur while_flag_1)))))) (set! jacobi_eigen_eigenvalues []) (set! jacobi_eigen_i 0) (while (< jacobi_eigen_i jacobi_eigen_n) (do (set! jacobi_eigen_eigenvalues (conj jacobi_eigen_eigenvalues (nth (nth jacobi_eigen_a jacobi_eigen_i) jacobi_eigen_i))) (set! jacobi_eigen_i (+ jacobi_eigen_i 1)))) (throw (ex-info "return" {:v {:values jacobi_eigen_eigenvalues :vectors jacobi_eigen_v}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matmul [matmul_a matmul_b]
  (binding [matmul_cols nil matmul_i nil matmul_inner nil matmul_j nil matmul_k nil matmul_m nil matmul_rows nil matmul_s nil] (try (do (set! matmul_rows (count matmul_a)) (set! matmul_cols (count (nth matmul_b 0))) (set! matmul_inner (count matmul_b)) (set! matmul_m (zeros_matrix matmul_rows matmul_cols)) (set! matmul_i 0) (while (< matmul_i matmul_rows) (do (set! matmul_j 0) (while (< matmul_j matmul_cols) (do (set! matmul_s 0.0) (set! matmul_k 0) (while (< matmul_k matmul_inner) (do (set! matmul_s (+ matmul_s (* (nth (nth matmul_a matmul_i) matmul_k) (nth (nth matmul_b matmul_k) matmul_j)))) (set! matmul_k (+ matmul_k 1)))) (set! matmul_m (assoc-in matmul_m [matmul_i matmul_j] matmul_s)) (set! matmul_j (+ matmul_j 1)))) (set! matmul_i (+ matmul_i 1)))) (throw (ex-info "return" {:v matmul_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_eigenpairs [sort_eigenpairs_vals sort_eigenpairs_vecs]
  (binding [sort_eigenpairs_i nil sort_eigenpairs_j nil sort_eigenpairs_n nil sort_eigenpairs_r nil sort_eigenpairs_tmp nil sort_eigenpairs_tv nil sort_eigenpairs_values nil sort_eigenpairs_vectors nil] (try (do (set! sort_eigenpairs_n (count sort_eigenpairs_vals)) (set! sort_eigenpairs_values sort_eigenpairs_vals) (set! sort_eigenpairs_vectors sort_eigenpairs_vecs) (set! sort_eigenpairs_i 0) (while (< sort_eigenpairs_i sort_eigenpairs_n) (do (set! sort_eigenpairs_j 0) (while (< sort_eigenpairs_j (- sort_eigenpairs_n 1)) (do (when (< (nth sort_eigenpairs_values sort_eigenpairs_j) (nth sort_eigenpairs_values (+ sort_eigenpairs_j 1))) (do (set! sort_eigenpairs_tmp (nth sort_eigenpairs_values sort_eigenpairs_j)) (set! sort_eigenpairs_values (assoc sort_eigenpairs_values sort_eigenpairs_j (nth sort_eigenpairs_values (+ sort_eigenpairs_j 1)))) (set! sort_eigenpairs_values (assoc sort_eigenpairs_values (+ sort_eigenpairs_j 1) sort_eigenpairs_tmp)) (set! sort_eigenpairs_r 0) (while (< sort_eigenpairs_r (count sort_eigenpairs_vectors)) (do (set! sort_eigenpairs_tv (nth (nth sort_eigenpairs_vectors sort_eigenpairs_r) sort_eigenpairs_j)) (set! sort_eigenpairs_vectors (assoc-in sort_eigenpairs_vectors [sort_eigenpairs_r sort_eigenpairs_j] (nth (nth sort_eigenpairs_vectors sort_eigenpairs_r) (+ sort_eigenpairs_j 1)))) (set! sort_eigenpairs_vectors (assoc-in sort_eigenpairs_vectors [sort_eigenpairs_r (+ sort_eigenpairs_j 1)] sort_eigenpairs_tv)) (set! sort_eigenpairs_r (+ sort_eigenpairs_r 1)))))) (set! sort_eigenpairs_j (+ sort_eigenpairs_j 1)))) (set! sort_eigenpairs_i (+ sort_eigenpairs_i 1)))) (throw (ex-info "return" {:v {:values sort_eigenpairs_values :vectors sort_eigenpairs_vectors}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_lanczos_eigenvectors [find_lanczos_eigenvectors_graph find_lanczos_eigenvectors_k]
  (binding [find_lanczos_eigenvectors_eig nil find_lanczos_eigenvectors_final_vectors nil find_lanczos_eigenvectors_res nil find_lanczos_eigenvectors_sorted nil] (try (do (alter-var-root (var find_lanczos_eigenvectors_graph) (constantly (validate_adjacency_list find_lanczos_eigenvectors_graph))) (set! find_lanczos_eigenvectors_res (lanczos_iteration find_lanczos_eigenvectors_graph find_lanczos_eigenvectors_k)) (set! find_lanczos_eigenvectors_eig (jacobi_eigen (:t find_lanczos_eigenvectors_res) 50)) (set! find_lanczos_eigenvectors_sorted (sort_eigenpairs (:values find_lanczos_eigenvectors_eig) (:vectors find_lanczos_eigenvectors_eig))) (set! find_lanczos_eigenvectors_final_vectors (matmul (:q find_lanczos_eigenvectors_res) (:vectors find_lanczos_eigenvectors_sorted))) (throw (ex-info "return" {:v {:values (:values find_lanczos_eigenvectors_sorted) :vectors find_lanczos_eigenvectors_final_vectors}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_arr]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_arr)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_arr list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_arr) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v (str list_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_to_string [matrix_to_string_m]
  (binding [matrix_to_string_i nil matrix_to_string_s nil] (try (do (set! matrix_to_string_s "[") (set! matrix_to_string_i 0) (while (< matrix_to_string_i (count matrix_to_string_m)) (do (set! matrix_to_string_s (str matrix_to_string_s (list_to_string (nth matrix_to_string_m matrix_to_string_i)))) (when (< matrix_to_string_i (- (count matrix_to_string_m) 1)) (set! matrix_to_string_s (str matrix_to_string_s "; "))) (set! matrix_to_string_i (+ matrix_to_string_i 1)))) (throw (ex-info "return" {:v (str matrix_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph [[1 2] [0 2] [0 1]])

(def ^:dynamic main_result (find_lanczos_eigenvectors main_graph 2))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (list_to_string (:values main_result)))
      (println (matrix_to_string (:vectors main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
