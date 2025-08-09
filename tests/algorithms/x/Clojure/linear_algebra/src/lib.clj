(ns main (:refer-clojure :exclude [rand random_int sqrtApprox arcsin_taylor acos_taylor vector_len vector_to_string vector_add vector_sub vector_eq vector_mul_scalar vector_dot vector_copy vector_component vector_change_component vector_euclidean_length vector_angle zero_vector unit_basis_vector axpy random_vector matrix_to_string matrix_add matrix_sub matrix_mul_vector matrix_mul_scalar matrix_component matrix_change_component matrix_minor matrix_cofactor matrix_determinant square_zero_matrix random_matrix main]))

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

(declare rand random_int sqrtApprox arcsin_taylor acos_taylor vector_len vector_to_string vector_add vector_sub vector_eq vector_mul_scalar vector_dot vector_copy vector_component vector_change_component vector_euclidean_length vector_angle zero_vector unit_basis_vector axpy random_vector matrix_to_string matrix_add matrix_sub matrix_mul_vector matrix_mul_scalar matrix_component matrix_change_component matrix_minor matrix_cofactor matrix_determinant square_zero_matrix random_matrix main)

(def ^:dynamic arcsin_taylor_den nil)

(def ^:dynamic arcsin_taylor_n nil)

(def ^:dynamic arcsin_taylor_num nil)

(def ^:dynamic arcsin_taylor_sum nil)

(def ^:dynamic arcsin_taylor_term nil)

(def ^:dynamic main_m nil)

(def ^:dynamic main_v1 nil)

(def ^:dynamic main_v2 nil)

(def ^:dynamic matrix_add_i nil)

(def ^:dynamic matrix_add_j nil)

(def ^:dynamic matrix_add_mat nil)

(def ^:dynamic matrix_add_row nil)

(def ^:dynamic matrix_change_component_data nil)

(def ^:dynamic matrix_cofactor_sign nil)

(def ^:dynamic matrix_determinant_sum nil)

(def ^:dynamic matrix_determinant_y nil)

(def ^:dynamic matrix_minor_i nil)

(def ^:dynamic matrix_minor_j nil)

(def ^:dynamic matrix_minor_minor nil)

(def ^:dynamic matrix_minor_row nil)

(def ^:dynamic matrix_minor_sub nil)

(def ^:dynamic matrix_mul_scalar_i nil)

(def ^:dynamic matrix_mul_scalar_j nil)

(def ^:dynamic matrix_mul_scalar_mat nil)

(def ^:dynamic matrix_mul_scalar_row nil)

(def ^:dynamic matrix_mul_vector_i nil)

(def ^:dynamic matrix_mul_vector_j nil)

(def ^:dynamic matrix_mul_vector_res nil)

(def ^:dynamic matrix_mul_vector_sum nil)

(def ^:dynamic matrix_sub_i nil)

(def ^:dynamic matrix_sub_j nil)

(def ^:dynamic matrix_sub_mat nil)

(def ^:dynamic matrix_sub_row nil)

(def ^:dynamic matrix_to_string_ans nil)

(def ^:dynamic matrix_to_string_i nil)

(def ^:dynamic matrix_to_string_j nil)

(def ^:dynamic random_int_r nil)

(def ^:dynamic random_matrix_i nil)

(def ^:dynamic random_matrix_j nil)

(def ^:dynamic random_matrix_mat nil)

(def ^:dynamic random_matrix_row nil)

(def ^:dynamic random_vector_i nil)

(def ^:dynamic random_vector_res nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic square_zero_matrix_i nil)

(def ^:dynamic square_zero_matrix_j nil)

(def ^:dynamic square_zero_matrix_mat nil)

(def ^:dynamic square_zero_matrix_row nil)

(def ^:dynamic unit_basis_vector_i nil)

(def ^:dynamic unit_basis_vector_res nil)

(def ^:dynamic vector_add_i nil)

(def ^:dynamic vector_add_res nil)

(def ^:dynamic vector_add_size nil)

(def ^:dynamic vector_angle_ang nil)

(def ^:dynamic vector_angle_den nil)

(def ^:dynamic vector_angle_num nil)

(def ^:dynamic vector_change_component_comps nil)

(def ^:dynamic vector_copy_i nil)

(def ^:dynamic vector_copy_res nil)

(def ^:dynamic vector_dot_i nil)

(def ^:dynamic vector_dot_size nil)

(def ^:dynamic vector_dot_sum nil)

(def ^:dynamic vector_eq_i nil)

(def ^:dynamic vector_euclidean_length_i nil)

(def ^:dynamic vector_euclidean_length_result nil)

(def ^:dynamic vector_euclidean_length_sum nil)

(def ^:dynamic vector_mul_scalar_i nil)

(def ^:dynamic vector_mul_scalar_res nil)

(def ^:dynamic vector_sub_i nil)

(def ^:dynamic vector_sub_res nil)

(def ^:dynamic vector_sub_size nil)

(def ^:dynamic vector_to_string_i nil)

(def ^:dynamic vector_to_string_s nil)

(def ^:dynamic zero_vector_i nil)

(def ^:dynamic zero_vector_res nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_seed 123456789)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random_int [random_int_a random_int_b]
  (binding [random_int_r nil] (try (do (set! random_int_r (mod (rand) (+ (- random_int_b random_int_a) 1))) (throw (ex-info "return" {:v (+ random_int_a random_int_r)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn arcsin_taylor [arcsin_taylor_x]
  (binding [arcsin_taylor_den nil arcsin_taylor_n nil arcsin_taylor_num nil arcsin_taylor_sum nil arcsin_taylor_term nil] (try (do (set! arcsin_taylor_term arcsin_taylor_x) (set! arcsin_taylor_sum arcsin_taylor_x) (set! arcsin_taylor_n 1) (while (< arcsin_taylor_n 10) (do (set! arcsin_taylor_num (* (* (* (* (- (* 2.0 (double arcsin_taylor_n)) 1.0) (- (* 2.0 (double arcsin_taylor_n)) 1.0)) arcsin_taylor_x) arcsin_taylor_x) arcsin_taylor_term)) (set! arcsin_taylor_den (* (* 2.0 (double arcsin_taylor_n)) (+ (* 2.0 (double arcsin_taylor_n)) 1.0))) (set! arcsin_taylor_term (quot arcsin_taylor_num arcsin_taylor_den)) (set! arcsin_taylor_sum (+ arcsin_taylor_sum arcsin_taylor_term)) (set! arcsin_taylor_n (+ arcsin_taylor_n 1)))) (throw (ex-info "return" {:v arcsin_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn acos_taylor [acos_taylor_x]
  (try (throw (ex-info "return" {:v (- (/ main_PI 2.0) (arcsin_taylor acos_taylor_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vector_len [vector_len_v]
  (try (throw (ex-info "return" {:v (count (:components vector_len_v))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vector_to_string [vector_to_string_v]
  (binding [vector_to_string_i nil vector_to_string_s nil] (try (do (set! vector_to_string_s "(") (set! vector_to_string_i 0) (while (< vector_to_string_i (count (:components vector_to_string_v))) (do (set! vector_to_string_s (str vector_to_string_s (str (get (:components vector_to_string_v) vector_to_string_i)))) (when (< vector_to_string_i (- (count (:components vector_to_string_v)) 1)) (set! vector_to_string_s (str vector_to_string_s ","))) (set! vector_to_string_i (+ vector_to_string_i 1)))) (set! vector_to_string_s (str vector_to_string_s ")")) (throw (ex-info "return" {:v vector_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_add [vector_add_a vector_add_b]
  (binding [vector_add_i nil vector_add_res nil vector_add_size nil] (try (do (set! vector_add_size (vector_len vector_add_a)) (when (not= vector_add_size (vector_len vector_add_b)) (throw (ex-info "return" {:v {:components []}}))) (set! vector_add_res []) (set! vector_add_i 0) (while (< vector_add_i vector_add_size) (do (set! vector_add_res (conj vector_add_res (+ (get (:components vector_add_a) vector_add_i) (get (:components vector_add_b) vector_add_i)))) (set! vector_add_i (+ vector_add_i 1)))) (throw (ex-info "return" {:v {:components vector_add_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_sub [vector_sub_a vector_sub_b]
  (binding [vector_sub_i nil vector_sub_res nil vector_sub_size nil] (try (do (set! vector_sub_size (vector_len vector_sub_a)) (when (not= vector_sub_size (vector_len vector_sub_b)) (throw (ex-info "return" {:v {:components []}}))) (set! vector_sub_res []) (set! vector_sub_i 0) (while (< vector_sub_i vector_sub_size) (do (set! vector_sub_res (conj vector_sub_res (- (get (:components vector_sub_a) vector_sub_i) (get (:components vector_sub_b) vector_sub_i)))) (set! vector_sub_i (+ vector_sub_i 1)))) (throw (ex-info "return" {:v {:components vector_sub_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_eq [vector_eq_a vector_eq_b]
  (binding [vector_eq_i nil] (try (do (when (not= (vector_len vector_eq_a) (vector_len vector_eq_b)) (throw (ex-info "return" {:v false}))) (set! vector_eq_i 0) (while (< vector_eq_i (vector_len vector_eq_a)) (do (when (not= (get (:components vector_eq_a) vector_eq_i) (get (:components vector_eq_b) vector_eq_i)) (throw (ex-info "return" {:v false}))) (set! vector_eq_i (+ vector_eq_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_mul_scalar [vector_mul_scalar_v vector_mul_scalar_s]
  (binding [vector_mul_scalar_i nil vector_mul_scalar_res nil] (try (do (set! vector_mul_scalar_res []) (set! vector_mul_scalar_i 0) (while (< vector_mul_scalar_i (vector_len vector_mul_scalar_v)) (do (set! vector_mul_scalar_res (conj vector_mul_scalar_res (* (get (:components vector_mul_scalar_v) vector_mul_scalar_i) vector_mul_scalar_s))) (set! vector_mul_scalar_i (+ vector_mul_scalar_i 1)))) (throw (ex-info "return" {:v {:components vector_mul_scalar_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_dot [vector_dot_a vector_dot_b]
  (binding [vector_dot_i nil vector_dot_size nil vector_dot_sum nil] (try (do (set! vector_dot_size (vector_len vector_dot_a)) (when (not= vector_dot_size (vector_len vector_dot_b)) (throw (ex-info "return" {:v 0.0}))) (set! vector_dot_sum 0.0) (set! vector_dot_i 0) (while (< vector_dot_i vector_dot_size) (do (set! vector_dot_sum (+ vector_dot_sum (* (get (:components vector_dot_a) vector_dot_i) (get (:components vector_dot_b) vector_dot_i)))) (set! vector_dot_i (+ vector_dot_i 1)))) (throw (ex-info "return" {:v vector_dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_copy [vector_copy_v]
  (binding [vector_copy_i nil vector_copy_res nil] (try (do (set! vector_copy_res []) (set! vector_copy_i 0) (while (< vector_copy_i (vector_len vector_copy_v)) (do (set! vector_copy_res (conj vector_copy_res (get (:components vector_copy_v) vector_copy_i))) (set! vector_copy_i (+ vector_copy_i 1)))) (throw (ex-info "return" {:v {:components vector_copy_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_component [vector_component_v vector_component_idx]
  (try (throw (ex-info "return" {:v (get (:components vector_component_v) vector_component_idx)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vector_change_component [vector_change_component_v vector_change_component_pos vector_change_component_value]
  (binding [vector_change_component_comps nil] (try (do (set! vector_change_component_comps (:components vector_change_component_v)) (set! vector_change_component_comps (assoc vector_change_component_comps vector_change_component_pos vector_change_component_value)) (throw (ex-info "return" {:v {:components vector_change_component_comps}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_euclidean_length [vector_euclidean_length_v]
  (binding [vector_euclidean_length_i nil vector_euclidean_length_result nil vector_euclidean_length_sum nil] (try (do (set! vector_euclidean_length_sum 0.0) (set! vector_euclidean_length_i 0) (while (< vector_euclidean_length_i (count (:components vector_euclidean_length_v))) (do (set! vector_euclidean_length_sum (+ vector_euclidean_length_sum (* (get (:components vector_euclidean_length_v) vector_euclidean_length_i) (get (:components vector_euclidean_length_v) vector_euclidean_length_i)))) (set! vector_euclidean_length_i (+ vector_euclidean_length_i 1)))) (set! vector_euclidean_length_result (sqrtApprox vector_euclidean_length_sum)) (throw (ex-info "return" {:v vector_euclidean_length_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_angle [vector_angle_a vector_angle_b vector_angle_deg]
  (binding [vector_angle_ang nil vector_angle_den nil vector_angle_num nil] (try (do (set! vector_angle_num (vector_dot vector_angle_a vector_angle_b)) (set! vector_angle_den (* (vector_euclidean_length vector_angle_a) (vector_euclidean_length vector_angle_b))) (set! vector_angle_ang (acos_taylor (quot vector_angle_num vector_angle_den))) (when vector_angle_deg (set! vector_angle_ang (/ (* vector_angle_ang 180.0) main_PI))) (throw (ex-info "return" {:v vector_angle_ang}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zero_vector [zero_vector_d]
  (binding [zero_vector_i nil zero_vector_res nil] (try (do (set! zero_vector_res []) (set! zero_vector_i 0) (while (< zero_vector_i zero_vector_d) (do (set! zero_vector_res (conj zero_vector_res 0.0)) (set! zero_vector_i (+ zero_vector_i 1)))) (throw (ex-info "return" {:v {:components zero_vector_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn unit_basis_vector [unit_basis_vector_d unit_basis_vector_pos]
  (binding [unit_basis_vector_i nil unit_basis_vector_res nil] (try (do (set! unit_basis_vector_res []) (set! unit_basis_vector_i 0) (while (< unit_basis_vector_i unit_basis_vector_d) (do (if (= unit_basis_vector_i unit_basis_vector_pos) (set! unit_basis_vector_res (conj unit_basis_vector_res 1.0)) (set! unit_basis_vector_res (conj unit_basis_vector_res 0.0))) (set! unit_basis_vector_i (+ unit_basis_vector_i 1)))) (throw (ex-info "return" {:v {:components unit_basis_vector_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn axpy [axpy_s axpy_x axpy_y]
  (try (throw (ex-info "return" {:v (vector_add (vector_mul_scalar axpy_x axpy_s) axpy_y)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random_vector [random_vector_n random_vector_a random_vector_b]
  (binding [random_vector_i nil random_vector_res nil] (try (do (set! random_vector_res []) (set! random_vector_i 0) (while (< random_vector_i random_vector_n) (do (set! random_vector_res (conj random_vector_res (double (random_int random_vector_a random_vector_b)))) (set! random_vector_i (+ random_vector_i 1)))) (throw (ex-info "return" {:v {:components random_vector_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_to_string [matrix_to_string_m]
  (binding [matrix_to_string_ans nil matrix_to_string_i nil matrix_to_string_j nil] (try (do (set! matrix_to_string_ans "") (set! matrix_to_string_i 0) (while (< matrix_to_string_i (:height matrix_to_string_m)) (do (set! matrix_to_string_ans (str matrix_to_string_ans "|")) (set! matrix_to_string_j 0) (while (< matrix_to_string_j (:width matrix_to_string_m)) (do (set! matrix_to_string_ans (str matrix_to_string_ans (str (nth (get (:data matrix_to_string_m) matrix_to_string_i) matrix_to_string_j)))) (when (< matrix_to_string_j (- (:width matrix_to_string_m) 1)) (set! matrix_to_string_ans (str matrix_to_string_ans ","))) (set! matrix_to_string_j (+ matrix_to_string_j 1)))) (set! matrix_to_string_ans (str matrix_to_string_ans "|\n")) (set! matrix_to_string_i (+ matrix_to_string_i 1)))) (throw (ex-info "return" {:v matrix_to_string_ans}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_add [matrix_add_a matrix_add_b]
  (binding [matrix_add_i nil matrix_add_j nil matrix_add_mat nil matrix_add_row nil] (try (do (when (or (not= (:width matrix_add_a) (:width matrix_add_b)) (not= (:height matrix_add_a) (:height matrix_add_b))) (throw (ex-info "return" {:v {:data [] :height 0 :width 0}}))) (set! matrix_add_mat []) (set! matrix_add_i 0) (while (< matrix_add_i (:height matrix_add_a)) (do (set! matrix_add_row []) (set! matrix_add_j 0) (while (< matrix_add_j (:width matrix_add_a)) (do (set! matrix_add_row (conj matrix_add_row (+ (nth (get (:data matrix_add_a) matrix_add_i) matrix_add_j) (nth (get (:data matrix_add_b) matrix_add_i) matrix_add_j)))) (set! matrix_add_j (+ matrix_add_j 1)))) (set! matrix_add_mat (conj matrix_add_mat matrix_add_row)) (set! matrix_add_i (+ matrix_add_i 1)))) (throw (ex-info "return" {:v {:data matrix_add_mat :height (:height matrix_add_a) :width (:width matrix_add_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_sub [matrix_sub_a matrix_sub_b]
  (binding [matrix_sub_i nil matrix_sub_j nil matrix_sub_mat nil matrix_sub_row nil] (try (do (when (or (not= (:width matrix_sub_a) (:width matrix_sub_b)) (not= (:height matrix_sub_a) (:height matrix_sub_b))) (throw (ex-info "return" {:v {:data [] :height 0 :width 0}}))) (set! matrix_sub_mat []) (set! matrix_sub_i 0) (while (< matrix_sub_i (:height matrix_sub_a)) (do (set! matrix_sub_row []) (set! matrix_sub_j 0) (while (< matrix_sub_j (:width matrix_sub_a)) (do (set! matrix_sub_row (conj matrix_sub_row (- (nth (get (:data matrix_sub_a) matrix_sub_i) matrix_sub_j) (nth (get (:data matrix_sub_b) matrix_sub_i) matrix_sub_j)))) (set! matrix_sub_j (+ matrix_sub_j 1)))) (set! matrix_sub_mat (conj matrix_sub_mat matrix_sub_row)) (set! matrix_sub_i (+ matrix_sub_i 1)))) (throw (ex-info "return" {:v {:data matrix_sub_mat :height (:height matrix_sub_a) :width (:width matrix_sub_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul_vector [matrix_mul_vector_m matrix_mul_vector_v]
  (binding [matrix_mul_vector_i nil matrix_mul_vector_j nil matrix_mul_vector_res nil matrix_mul_vector_sum nil] (try (do (when (not= (count (:components matrix_mul_vector_v)) (:width matrix_mul_vector_m)) (throw (ex-info "return" {:v {:components []}}))) (set! matrix_mul_vector_res (zero_vector (:height matrix_mul_vector_m))) (set! matrix_mul_vector_i 0) (while (< matrix_mul_vector_i (:height matrix_mul_vector_m)) (do (set! matrix_mul_vector_sum 0.0) (set! matrix_mul_vector_j 0) (while (< matrix_mul_vector_j (:width matrix_mul_vector_m)) (do (set! matrix_mul_vector_sum (+ matrix_mul_vector_sum (* (nth (get (:data matrix_mul_vector_m) matrix_mul_vector_i) matrix_mul_vector_j) (get (:components matrix_mul_vector_v) matrix_mul_vector_j)))) (set! matrix_mul_vector_j (+ matrix_mul_vector_j 1)))) (set! matrix_mul_vector_res (vector_change_component matrix_mul_vector_res matrix_mul_vector_i matrix_mul_vector_sum)) (set! matrix_mul_vector_i (+ matrix_mul_vector_i 1)))) (throw (ex-info "return" {:v matrix_mul_vector_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul_scalar [matrix_mul_scalar_m matrix_mul_scalar_s]
  (binding [matrix_mul_scalar_i nil matrix_mul_scalar_j nil matrix_mul_scalar_mat nil matrix_mul_scalar_row nil] (try (do (set! matrix_mul_scalar_mat []) (set! matrix_mul_scalar_i 0) (while (< matrix_mul_scalar_i (:height matrix_mul_scalar_m)) (do (set! matrix_mul_scalar_row []) (set! matrix_mul_scalar_j 0) (while (< matrix_mul_scalar_j (:width matrix_mul_scalar_m)) (do (set! matrix_mul_scalar_row (conj matrix_mul_scalar_row (* (nth (get (:data matrix_mul_scalar_m) matrix_mul_scalar_i) matrix_mul_scalar_j) matrix_mul_scalar_s))) (set! matrix_mul_scalar_j (+ matrix_mul_scalar_j 1)))) (set! matrix_mul_scalar_mat (conj matrix_mul_scalar_mat matrix_mul_scalar_row)) (set! matrix_mul_scalar_i (+ matrix_mul_scalar_i 1)))) (throw (ex-info "return" {:v {:data matrix_mul_scalar_mat :height (:height matrix_mul_scalar_m) :width (:width matrix_mul_scalar_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_component [matrix_component_m matrix_component_x matrix_component_y]
  (try (throw (ex-info "return" {:v (nth (get (:data matrix_component_m) matrix_component_x) matrix_component_y)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn matrix_change_component [matrix_change_component_m matrix_change_component_x matrix_change_component_y matrix_change_component_value]
  (binding [matrix_change_component_data nil] (try (do (set! matrix_change_component_data (:data matrix_change_component_m)) (set! matrix_change_component_data (assoc-in matrix_change_component_data [matrix_change_component_x matrix_change_component_y] matrix_change_component_value)) (throw (ex-info "return" {:v {:data matrix_change_component_data :height (:height matrix_change_component_m) :width (:width matrix_change_component_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_minor [matrix_minor_m matrix_minor_x matrix_minor_y]
  (binding [matrix_minor_i nil matrix_minor_j nil matrix_minor_minor nil matrix_minor_row nil matrix_minor_sub nil] (try (do (when (not= (:height matrix_minor_m) (:width matrix_minor_m)) (throw (ex-info "return" {:v 0.0}))) (set! matrix_minor_minor []) (set! matrix_minor_i 0) (while (< matrix_minor_i (:height matrix_minor_m)) (do (when (not= matrix_minor_i matrix_minor_x) (do (set! matrix_minor_row []) (set! matrix_minor_j 0) (while (< matrix_minor_j (:width matrix_minor_m)) (do (when (not= matrix_minor_j matrix_minor_y) (set! matrix_minor_row (conj matrix_minor_row (nth (get (:data matrix_minor_m) matrix_minor_i) matrix_minor_j)))) (set! matrix_minor_j (+ matrix_minor_j 1)))) (set! matrix_minor_minor (conj matrix_minor_minor matrix_minor_row)))) (set! matrix_minor_i (+ matrix_minor_i 1)))) (set! matrix_minor_sub {:data matrix_minor_minor :height (- (:height matrix_minor_m) 1) :width (- (:width matrix_minor_m) 1)}) (throw (ex-info "return" {:v (matrix_determinant matrix_minor_sub)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_cofactor [matrix_cofactor_m matrix_cofactor_x matrix_cofactor_y]
  (binding [matrix_cofactor_sign nil] (try (do (set! matrix_cofactor_sign (if (= (mod (+ matrix_cofactor_x matrix_cofactor_y) 2) 0) 1.0 (- 1.0))) (throw (ex-info "return" {:v (* matrix_cofactor_sign (matrix_minor matrix_cofactor_m matrix_cofactor_x matrix_cofactor_y))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_determinant [matrix_determinant_m]
  (binding [matrix_determinant_sum nil matrix_determinant_y nil] (try (do (when (not= (:height matrix_determinant_m) (:width matrix_determinant_m)) (throw (ex-info "return" {:v 0.0}))) (when (= (:height matrix_determinant_m) 0) (throw (ex-info "return" {:v 0.0}))) (when (= (:height matrix_determinant_m) 1) (throw (ex-info "return" {:v (nth (get (:data matrix_determinant_m) 0) 0)}))) (when (= (:height matrix_determinant_m) 2) (throw (ex-info "return" {:v (- (* (nth (get (:data matrix_determinant_m) 0) 0) (nth (get (:data matrix_determinant_m) 1) 1)) (* (nth (get (:data matrix_determinant_m) 0) 1) (nth (get (:data matrix_determinant_m) 1) 0)))}))) (set! matrix_determinant_sum 0.0) (set! matrix_determinant_y 0) (while (< matrix_determinant_y (:width matrix_determinant_m)) (do (set! matrix_determinant_sum (+ matrix_determinant_sum (* (nth (get (:data matrix_determinant_m) 0) matrix_determinant_y) (matrix_cofactor matrix_determinant_m 0 matrix_determinant_y)))) (set! matrix_determinant_y (+ matrix_determinant_y 1)))) (throw (ex-info "return" {:v matrix_determinant_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn square_zero_matrix [square_zero_matrix_n]
  (binding [square_zero_matrix_i nil square_zero_matrix_j nil square_zero_matrix_mat nil square_zero_matrix_row nil] (try (do (set! square_zero_matrix_mat []) (set! square_zero_matrix_i 0) (while (< square_zero_matrix_i square_zero_matrix_n) (do (set! square_zero_matrix_row []) (set! square_zero_matrix_j 0) (while (< square_zero_matrix_j square_zero_matrix_n) (do (set! square_zero_matrix_row (conj square_zero_matrix_row 0.0)) (set! square_zero_matrix_j (+ square_zero_matrix_j 1)))) (set! square_zero_matrix_mat (conj square_zero_matrix_mat square_zero_matrix_row)) (set! square_zero_matrix_i (+ square_zero_matrix_i 1)))) (throw (ex-info "return" {:v {:data square_zero_matrix_mat :height square_zero_matrix_n :width square_zero_matrix_n}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn random_matrix [random_matrix_w random_matrix_h random_matrix_a random_matrix_b]
  (binding [random_matrix_i nil random_matrix_j nil random_matrix_mat nil random_matrix_row nil] (try (do (set! random_matrix_mat []) (set! random_matrix_i 0) (while (< random_matrix_i random_matrix_h) (do (set! random_matrix_row []) (set! random_matrix_j 0) (while (< random_matrix_j random_matrix_w) (do (set! random_matrix_row (conj random_matrix_row (double (random_int random_matrix_a random_matrix_b)))) (set! random_matrix_j (+ random_matrix_j 1)))) (set! random_matrix_mat (conj random_matrix_mat random_matrix_row)) (set! random_matrix_i (+ random_matrix_i 1)))) (throw (ex-info "return" {:v {:data random_matrix_mat :height random_matrix_h :width random_matrix_w}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_m nil main_v1 nil main_v2 nil] (do (set! main_v1 {:components [1.0 2.0 3.0]}) (set! main_v2 {:components [4.0 5.0 6.0]}) (println (vector_to_string (vector_add main_v1 main_v2))) (println (str (vector_dot main_v1 main_v2))) (println (str (vector_euclidean_length main_v1))) (set! main_m {:data [[1.0 2.0] [3.0 4.0]] :height 2 :width 2}) (println (str (matrix_determinant main_m))))))

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
