(ns main (:refer-clojure :exclude [int_to_string float_to_string vector_component vector_str_int vector_str_float vector_add vector_sub vector_scalar_mul vector_dot sqrt_newton euclidean_length zero_vector unit_basis_vector axpy copy_vector change_component matrix_str submatrix determinant matrix_minor matrix_cofactor matrix_mul_vector matrix_mul_scalar matrix_change_component matrix_component matrix_add matrix_sub square_zero_matrix assert_int assert_str assert_float]))

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

(declare int_to_string float_to_string vector_component vector_str_int vector_str_float vector_add vector_sub vector_scalar_mul vector_dot sqrt_newton euclidean_length zero_vector unit_basis_vector axpy copy_vector change_component matrix_str submatrix determinant matrix_minor matrix_cofactor matrix_mul_vector matrix_mul_scalar matrix_change_component matrix_component matrix_add matrix_sub square_zero_matrix assert_int assert_str assert_float)

(def ^:dynamic assert_float_diff nil)

(def ^:dynamic axpy_i nil)

(def ^:dynamic axpy_res nil)

(def ^:dynamic change_component_v nil)

(def ^:dynamic copy_vector_i nil)

(def ^:dynamic copy_vector_res nil)

(def ^:dynamic determinant_c nil)

(def ^:dynamic determinant_det nil)

(def ^:dynamic determinant_n nil)

(def ^:dynamic determinant_sign nil)

(def ^:dynamic determinant_sub nil)

(def ^:dynamic euclidean_length_i nil)

(def ^:dynamic euclidean_length_sum nil)

(def ^:dynamic euclidean_length_val nil)

(def ^:dynamic float_to_string_digit nil)

(def ^:dynamic float_to_string_frac nil)

(def ^:dynamic float_to_string_i nil)

(def ^:dynamic float_to_string_int_part nil)

(def ^:dynamic float_to_string_neg nil)

(def ^:dynamic float_to_string_num nil)

(def ^:dynamic float_to_string_res nil)

(def ^:dynamic int_to_string_ch nil)

(def ^:dynamic int_to_string_digit nil)

(def ^:dynamic int_to_string_neg nil)

(def ^:dynamic int_to_string_num nil)

(def ^:dynamic int_to_string_res nil)

(def ^:dynamic main_zcount nil)

(def ^:dynamic main_zi nil)

(def ^:dynamic matrix_add_i nil)

(def ^:dynamic matrix_add_j nil)

(def ^:dynamic matrix_add_res nil)

(def ^:dynamic matrix_add_row nil)

(def ^:dynamic matrix_change_component_m nil)

(def ^:dynamic matrix_cofactor_sign nil)

(def ^:dynamic matrix_mul_scalar_i nil)

(def ^:dynamic matrix_mul_scalar_j nil)

(def ^:dynamic matrix_mul_scalar_res nil)

(def ^:dynamic matrix_mul_scalar_row nil)

(def ^:dynamic matrix_mul_vector_i nil)

(def ^:dynamic matrix_mul_vector_j nil)

(def ^:dynamic matrix_mul_vector_res nil)

(def ^:dynamic matrix_mul_vector_sum nil)

(def ^:dynamic matrix_str_i nil)

(def ^:dynamic matrix_str_j nil)

(def ^:dynamic matrix_str_s nil)

(def ^:dynamic matrix_sub_i nil)

(def ^:dynamic matrix_sub_j nil)

(def ^:dynamic matrix_sub_res nil)

(def ^:dynamic matrix_sub_row nil)

(def ^:dynamic sqrt_newton_high nil)

(def ^:dynamic sqrt_newton_i nil)

(def ^:dynamic sqrt_newton_low nil)

(def ^:dynamic sqrt_newton_mid nil)

(def ^:dynamic square_zero_matrix_i nil)

(def ^:dynamic square_zero_matrix_m nil)

(def ^:dynamic submatrix_i nil)

(def ^:dynamic submatrix_j nil)

(def ^:dynamic submatrix_r nil)

(def ^:dynamic submatrix_res nil)

(def ^:dynamic unit_basis_vector_v nil)

(def ^:dynamic vector_add_i nil)

(def ^:dynamic vector_add_res nil)

(def ^:dynamic vector_dot_i nil)

(def ^:dynamic vector_dot_sum nil)

(def ^:dynamic vector_scalar_mul_i nil)

(def ^:dynamic vector_scalar_mul_res nil)

(def ^:dynamic vector_str_float_i nil)

(def ^:dynamic vector_str_float_s nil)

(def ^:dynamic vector_str_int_i nil)

(def ^:dynamic vector_str_int_s nil)

(def ^:dynamic vector_sub_i nil)

(def ^:dynamic vector_sub_res nil)

(def ^:dynamic zero_vector_i nil)

(def ^:dynamic zero_vector_v nil)

(defn int_to_string [int_to_string_n]
  (binding [int_to_string_ch nil int_to_string_digit nil int_to_string_neg nil int_to_string_num nil int_to_string_res nil] (try (do (when (= int_to_string_n 0) (throw (ex-info "return" {:v "0"}))) (set! int_to_string_num int_to_string_n) (set! int_to_string_neg false) (when (< int_to_string_num 0) (do (set! int_to_string_neg true) (set! int_to_string_num (- int_to_string_num)))) (set! int_to_string_res "") (while (> int_to_string_num 0) (do (set! int_to_string_digit (mod int_to_string_num 10)) (set! int_to_string_ch (subs "0123456789" int_to_string_digit (min (+ int_to_string_digit 1) (count "0123456789")))) (set! int_to_string_res (str int_to_string_ch int_to_string_res)) (set! int_to_string_num (quot int_to_string_num 10)))) (when int_to_string_neg (set! int_to_string_res (str "-" int_to_string_res))) (throw (ex-info "return" {:v int_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn float_to_string [float_to_string_x float_to_string_dec]
  (binding [float_to_string_digit nil float_to_string_frac nil float_to_string_i nil float_to_string_int_part nil float_to_string_neg nil float_to_string_num nil float_to_string_res nil] (try (do (set! float_to_string_neg false) (set! float_to_string_num float_to_string_x) (when (< float_to_string_num 0.0) (do (set! float_to_string_neg true) (set! float_to_string_num (- float_to_string_num)))) (set! float_to_string_int_part (long float_to_string_num)) (set! float_to_string_res (int_to_string float_to_string_int_part)) (when (> float_to_string_dec 0) (do (set! float_to_string_res (str float_to_string_res ".")) (set! float_to_string_frac (- float_to_string_num (double float_to_string_int_part))) (set! float_to_string_i 0) (while (< float_to_string_i float_to_string_dec) (do (set! float_to_string_frac (* float_to_string_frac 10.0)) (set! float_to_string_digit (long float_to_string_frac)) (set! float_to_string_res (str float_to_string_res (subs "0123456789" float_to_string_digit (min (+ float_to_string_digit 1) (count "0123456789"))))) (set! float_to_string_frac (- float_to_string_frac (double float_to_string_digit))) (set! float_to_string_i (+ float_to_string_i 1)))))) (when float_to_string_neg (set! float_to_string_res (str "-" float_to_string_res))) (throw (ex-info "return" {:v float_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_component [vector_component_v vector_component_i]
  (try (throw (ex-info "return" {:v (nth vector_component_v vector_component_i)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vector_str_int [vector_str_int_v]
  (binding [vector_str_int_i nil vector_str_int_s nil] (try (do (set! vector_str_int_s "(") (set! vector_str_int_i 0) (while (< vector_str_int_i (count vector_str_int_v)) (do (set! vector_str_int_s (str vector_str_int_s (int_to_string (nth vector_str_int_v vector_str_int_i)))) (when (< (+ vector_str_int_i 1) (count vector_str_int_v)) (set! vector_str_int_s (str vector_str_int_s ","))) (set! vector_str_int_i (+ vector_str_int_i 1)))) (set! vector_str_int_s (str vector_str_int_s ")")) (throw (ex-info "return" {:v vector_str_int_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_str_float [vector_str_float_v vector_str_float_dec]
  (binding [vector_str_float_i nil vector_str_float_s nil] (try (do (set! vector_str_float_s "(") (set! vector_str_float_i 0) (while (< vector_str_float_i (count vector_str_float_v)) (do (set! vector_str_float_s (str vector_str_float_s (float_to_string (nth vector_str_float_v vector_str_float_i) vector_str_float_dec))) (when (< (+ vector_str_float_i 1) (count vector_str_float_v)) (set! vector_str_float_s (str vector_str_float_s ","))) (set! vector_str_float_i (+ vector_str_float_i 1)))) (set! vector_str_float_s (str vector_str_float_s ")")) (throw (ex-info "return" {:v vector_str_float_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_add [vector_add_a vector_add_b]
  (binding [vector_add_i nil vector_add_res nil] (try (do (set! vector_add_res []) (set! vector_add_i 0) (while (< vector_add_i (count vector_add_a)) (do (set! vector_add_res (conj vector_add_res (+ (nth vector_add_a vector_add_i) (nth vector_add_b vector_add_i)))) (set! vector_add_i (+ vector_add_i 1)))) (throw (ex-info "return" {:v vector_add_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_sub [vector_sub_a vector_sub_b]
  (binding [vector_sub_i nil vector_sub_res nil] (try (do (set! vector_sub_res []) (set! vector_sub_i 0) (while (< vector_sub_i (count vector_sub_a)) (do (set! vector_sub_res (conj vector_sub_res (- (nth vector_sub_a vector_sub_i) (nth vector_sub_b vector_sub_i)))) (set! vector_sub_i (+ vector_sub_i 1)))) (throw (ex-info "return" {:v vector_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_scalar_mul [vector_scalar_mul_v vector_scalar_mul_s]
  (binding [vector_scalar_mul_i nil vector_scalar_mul_res nil] (try (do (set! vector_scalar_mul_res []) (set! vector_scalar_mul_i 0) (while (< vector_scalar_mul_i (count vector_scalar_mul_v)) (do (set! vector_scalar_mul_res (conj vector_scalar_mul_res (* (double (nth vector_scalar_mul_v vector_scalar_mul_i)) vector_scalar_mul_s))) (set! vector_scalar_mul_i (+ vector_scalar_mul_i 1)))) (throw (ex-info "return" {:v vector_scalar_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vector_dot [vector_dot_a vector_dot_b]
  (binding [vector_dot_i nil vector_dot_sum nil] (try (do (set! vector_dot_sum 0) (set! vector_dot_i 0) (while (< vector_dot_i (count vector_dot_a)) (do (set! vector_dot_sum (+ vector_dot_sum (* (nth vector_dot_a vector_dot_i) (nth vector_dot_b vector_dot_i)))) (set! vector_dot_i (+ vector_dot_i 1)))) (throw (ex-info "return" {:v vector_dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrt_newton [sqrt_newton_x]
  (binding [sqrt_newton_high nil sqrt_newton_i nil sqrt_newton_low nil sqrt_newton_mid nil] (try (do (when (= sqrt_newton_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_newton_low 0.0) (set! sqrt_newton_high sqrt_newton_x) (when (< sqrt_newton_x 1.0) (set! sqrt_newton_high 1.0)) (set! sqrt_newton_mid 0.0) (set! sqrt_newton_i 0) (while (< sqrt_newton_i 40) (do (set! sqrt_newton_mid (/ (+ sqrt_newton_low sqrt_newton_high) 2.0)) (if (> (* sqrt_newton_mid sqrt_newton_mid) sqrt_newton_x) (set! sqrt_newton_high sqrt_newton_mid) (set! sqrt_newton_low sqrt_newton_mid)) (set! sqrt_newton_i (+ sqrt_newton_i 1)))) (throw (ex-info "return" {:v sqrt_newton_mid}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euclidean_length [euclidean_length_v]
  (binding [euclidean_length_i nil euclidean_length_sum nil euclidean_length_val nil] (try (do (set! euclidean_length_sum 0.0) (set! euclidean_length_i 0) (while (< euclidean_length_i (count euclidean_length_v)) (do (set! euclidean_length_val (double (nth euclidean_length_v euclidean_length_i))) (set! euclidean_length_sum (+ euclidean_length_sum (* euclidean_length_val euclidean_length_val))) (set! euclidean_length_i (+ euclidean_length_i 1)))) (throw (ex-info "return" {:v (sqrt_newton euclidean_length_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zero_vector [zero_vector_n]
  (binding [zero_vector_i nil zero_vector_v nil] (try (do (set! zero_vector_v []) (set! zero_vector_i 0) (while (< zero_vector_i zero_vector_n) (do (set! zero_vector_v (conj zero_vector_v 0)) (set! zero_vector_i (+ zero_vector_i 1)))) (throw (ex-info "return" {:v zero_vector_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn unit_basis_vector [unit_basis_vector_n unit_basis_vector_idx]
  (binding [unit_basis_vector_v nil] (try (do (set! unit_basis_vector_v (zero_vector unit_basis_vector_n)) (set! unit_basis_vector_v (assoc unit_basis_vector_v unit_basis_vector_idx 1)) (throw (ex-info "return" {:v unit_basis_vector_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn axpy [axpy_a axpy_x axpy_y]
  (binding [axpy_i nil axpy_res nil] (try (do (set! axpy_res []) (set! axpy_i 0) (while (< axpy_i (count axpy_x)) (do (set! axpy_res (conj axpy_res (+ (* axpy_a (nth axpy_x axpy_i)) (nth axpy_y axpy_i)))) (set! axpy_i (+ axpy_i 1)))) (throw (ex-info "return" {:v axpy_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn copy_vector [copy_vector_x]
  (binding [copy_vector_i nil copy_vector_res nil] (try (do (set! copy_vector_res []) (set! copy_vector_i 0) (while (< copy_vector_i (count copy_vector_x)) (do (set! copy_vector_res (conj copy_vector_res (nth copy_vector_x copy_vector_i))) (set! copy_vector_i (+ copy_vector_i 1)))) (throw (ex-info "return" {:v copy_vector_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn change_component [change_component_v_p change_component_idx change_component_val]
  (binding [change_component_v nil] (do (set! change_component_v change_component_v_p) (set! change_component_v (assoc change_component_v change_component_idx change_component_val)) change_component_v)))

(defn matrix_str [matrix_str_m]
  (binding [matrix_str_i nil matrix_str_j nil matrix_str_s nil] (try (do (set! matrix_str_s "") (set! matrix_str_i 0) (while (< matrix_str_i (count matrix_str_m)) (do (set! matrix_str_s (str matrix_str_s "|")) (set! matrix_str_j 0) (while (< matrix_str_j (count (nth matrix_str_m 0))) (do (set! matrix_str_s (str matrix_str_s (int_to_string (nth (nth matrix_str_m matrix_str_i) matrix_str_j)))) (when (< (+ matrix_str_j 1) (count (nth matrix_str_m 0))) (set! matrix_str_s (str matrix_str_s ","))) (set! matrix_str_j (+ matrix_str_j 1)))) (set! matrix_str_s (str matrix_str_s "|\n")) (set! matrix_str_i (+ matrix_str_i 1)))) (throw (ex-info "return" {:v matrix_str_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn submatrix [submatrix_m submatrix_row submatrix_col]
  (binding [submatrix_i nil submatrix_j nil submatrix_r nil submatrix_res nil] (try (do (set! submatrix_res []) (set! submatrix_i 0) (while (< submatrix_i (count submatrix_m)) (do (when (not= submatrix_i submatrix_row) (do (set! submatrix_r []) (set! submatrix_j 0) (while (< submatrix_j (count (nth submatrix_m 0))) (do (when (not= submatrix_j submatrix_col) (set! submatrix_r (conj submatrix_r (nth (nth submatrix_m submatrix_i) submatrix_j)))) (set! submatrix_j (+ submatrix_j 1)))) (set! submatrix_res (conj submatrix_res submatrix_r)))) (set! submatrix_i (+ submatrix_i 1)))) (throw (ex-info "return" {:v submatrix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn determinant [determinant_m]
  (binding [determinant_c nil determinant_det nil determinant_n nil determinant_sign nil determinant_sub nil] (try (do (set! determinant_n (count determinant_m)) (when (= determinant_n 1) (throw (ex-info "return" {:v (nth (nth determinant_m 0) 0)}))) (when (= determinant_n 2) (throw (ex-info "return" {:v (- (* (nth (nth determinant_m 0) 0) (nth (nth determinant_m 1) 1)) (* (nth (nth determinant_m 0) 1) (nth (nth determinant_m 1) 0)))}))) (set! determinant_det 0) (set! determinant_c 0) (while (< determinant_c determinant_n) (do (set! determinant_sub (submatrix determinant_m 0 determinant_c)) (set! determinant_sign 1) (when (= (mod determinant_c 2) 1) (set! determinant_sign (- 1))) (set! determinant_det (+ determinant_det (* (* determinant_sign (nth (nth determinant_m 0) determinant_c)) (determinant determinant_sub)))) (set! determinant_c (+ determinant_c 1)))) (throw (ex-info "return" {:v determinant_det}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_minor [matrix_minor_m matrix_minor_row matrix_minor_col]
  (try (throw (ex-info "return" {:v (determinant (submatrix matrix_minor_m matrix_minor_row matrix_minor_col))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn matrix_cofactor [matrix_cofactor_m matrix_cofactor_row matrix_cofactor_col]
  (binding [matrix_cofactor_sign nil] (try (do (set! matrix_cofactor_sign 1) (when (= (mod (+ matrix_cofactor_row matrix_cofactor_col) 2) 1) (set! matrix_cofactor_sign (- 1))) (throw (ex-info "return" {:v (* matrix_cofactor_sign (matrix_minor matrix_cofactor_m matrix_cofactor_row matrix_cofactor_col))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul_vector [matrix_mul_vector_m matrix_mul_vector_v]
  (binding [matrix_mul_vector_i nil matrix_mul_vector_j nil matrix_mul_vector_res nil matrix_mul_vector_sum nil] (try (do (set! matrix_mul_vector_res []) (set! matrix_mul_vector_i 0) (while (< matrix_mul_vector_i (count matrix_mul_vector_m)) (do (set! matrix_mul_vector_sum 0) (set! matrix_mul_vector_j 0) (while (< matrix_mul_vector_j (count (nth matrix_mul_vector_m 0))) (do (set! matrix_mul_vector_sum (+ matrix_mul_vector_sum (* (nth (nth matrix_mul_vector_m matrix_mul_vector_i) matrix_mul_vector_j) (nth matrix_mul_vector_v matrix_mul_vector_j)))) (set! matrix_mul_vector_j (+ matrix_mul_vector_j 1)))) (set! matrix_mul_vector_res (conj matrix_mul_vector_res matrix_mul_vector_sum)) (set! matrix_mul_vector_i (+ matrix_mul_vector_i 1)))) (throw (ex-info "return" {:v matrix_mul_vector_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul_scalar [matrix_mul_scalar_m matrix_mul_scalar_s]
  (binding [matrix_mul_scalar_i nil matrix_mul_scalar_j nil matrix_mul_scalar_res nil matrix_mul_scalar_row nil] (try (do (set! matrix_mul_scalar_res []) (set! matrix_mul_scalar_i 0) (while (< matrix_mul_scalar_i (count matrix_mul_scalar_m)) (do (set! matrix_mul_scalar_row []) (set! matrix_mul_scalar_j 0) (while (< matrix_mul_scalar_j (count (nth matrix_mul_scalar_m 0))) (do (set! matrix_mul_scalar_row (conj matrix_mul_scalar_row (* (nth (nth matrix_mul_scalar_m matrix_mul_scalar_i) matrix_mul_scalar_j) matrix_mul_scalar_s))) (set! matrix_mul_scalar_j (+ matrix_mul_scalar_j 1)))) (set! matrix_mul_scalar_res (conj matrix_mul_scalar_res matrix_mul_scalar_row)) (set! matrix_mul_scalar_i (+ matrix_mul_scalar_i 1)))) (throw (ex-info "return" {:v matrix_mul_scalar_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_change_component [matrix_change_component_m_p matrix_change_component_i matrix_change_component_j matrix_change_component_val]
  (binding [matrix_change_component_m nil] (do (set! matrix_change_component_m matrix_change_component_m_p) (set! matrix_change_component_m (assoc-in matrix_change_component_m [matrix_change_component_i matrix_change_component_j] matrix_change_component_val)) matrix_change_component_m)))

(defn matrix_component [matrix_component_m matrix_component_i matrix_component_j]
  (try (throw (ex-info "return" {:v (nth (nth matrix_component_m matrix_component_i) matrix_component_j)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn matrix_add [matrix_add_a matrix_add_b]
  (binding [matrix_add_i nil matrix_add_j nil matrix_add_res nil matrix_add_row nil] (try (do (set! matrix_add_res []) (set! matrix_add_i 0) (while (< matrix_add_i (count matrix_add_a)) (do (set! matrix_add_row []) (set! matrix_add_j 0) (while (< matrix_add_j (count (nth matrix_add_a 0))) (do (set! matrix_add_row (conj matrix_add_row (+ (nth (nth matrix_add_a matrix_add_i) matrix_add_j) (nth (nth matrix_add_b matrix_add_i) matrix_add_j)))) (set! matrix_add_j (+ matrix_add_j 1)))) (set! matrix_add_res (conj matrix_add_res matrix_add_row)) (set! matrix_add_i (+ matrix_add_i 1)))) (throw (ex-info "return" {:v matrix_add_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_sub [matrix_sub_a matrix_sub_b]
  (binding [matrix_sub_i nil matrix_sub_j nil matrix_sub_res nil matrix_sub_row nil] (try (do (set! matrix_sub_res []) (set! matrix_sub_i 0) (while (< matrix_sub_i (count matrix_sub_a)) (do (set! matrix_sub_row []) (set! matrix_sub_j 0) (while (< matrix_sub_j (count (nth matrix_sub_a 0))) (do (set! matrix_sub_row (conj matrix_sub_row (- (nth (nth matrix_sub_a matrix_sub_i) matrix_sub_j) (nth (nth matrix_sub_b matrix_sub_i) matrix_sub_j)))) (set! matrix_sub_j (+ matrix_sub_j 1)))) (set! matrix_sub_res (conj matrix_sub_res matrix_sub_row)) (set! matrix_sub_i (+ matrix_sub_i 1)))) (throw (ex-info "return" {:v matrix_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn square_zero_matrix [square_zero_matrix_n]
  (binding [square_zero_matrix_i nil square_zero_matrix_m nil] (try (do (set! square_zero_matrix_m []) (set! square_zero_matrix_i 0) (while (< square_zero_matrix_i square_zero_matrix_n) (do (set! square_zero_matrix_m (conj square_zero_matrix_m (zero_vector square_zero_matrix_n))) (set! square_zero_matrix_i (+ square_zero_matrix_i 1)))) (throw (ex-info "return" {:v square_zero_matrix_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn assert_int [assert_int_name assert_int_actual assert_int_expected]
  (do (if (= assert_int_actual assert_int_expected) (println (str assert_int_name " ok")) (println (str (str (str (str assert_int_name " fail ") (int_to_string assert_int_actual)) " != ") (int_to_string assert_int_expected)))) assert_int_name))

(defn assert_str [assert_str_name assert_str_actual assert_str_expected]
  (do (if (= assert_str_actual assert_str_expected) (println (str assert_str_name " ok")) (do (println (str assert_str_name " fail")) (println assert_str_actual) (println assert_str_expected))) assert_str_name))

(defn assert_float [assert_float_name assert_float_actual assert_float_expected assert_float_eps]
  (binding [assert_float_diff nil] (do (set! assert_float_diff (- assert_float_actual assert_float_expected)) (when (< assert_float_diff 0.0) (set! assert_float_diff (- assert_float_diff))) (if (<= assert_float_diff assert_float_eps) (println (str assert_float_name " ok")) (println (str assert_float_name " fail"))) assert_float_name)))

(def ^:dynamic main_vx [1 2 3])

(def ^:dynamic main_vs [0 0 0 0 0 1])

(def ^:dynamic main_vsize [1 2 3 4])

(def ^:dynamic main_va [1 2 3])

(def ^:dynamic main_vb [1 1 1])

(def ^:dynamic main_vsum (vector_add main_va main_vb))

(def ^:dynamic main_vsub (vector_sub main_va main_vb))

(def ^:dynamic main_vmul (vector_scalar_mul main_va 3.0))

(def ^:dynamic main_zvec (zero_vector 10))

(def ^:dynamic main_zstr (vector_str_int main_zvec))

(def ^:dynamic main_zcount 0)

(def ^:dynamic main_zi 0)

(def ^:dynamic main_vcopy (copy_vector [1 0 0 0 0 0]))

(def ^:dynamic main_vchange [1 0 0])

(def ^:dynamic main_ma [[1 2 3] [2 4 5] [6 7 8]])

(def ^:dynamic main_mb [[1 2 3] [4 5 6] [7 8 9]])

(def ^:dynamic main_mv (matrix_mul_vector main_mb [1 2 3]))

(def ^:dynamic main_msc (matrix_mul_scalar main_mb 2))

(def ^:dynamic main_mc [[1 2 3] [2 4 5] [6 7 8]])

(def ^:dynamic main_madd (matrix_add [[1 2 3] [2 4 5] [6 7 8]] [[1 2 7] [2 4 5] [6 7 10]]))

(def ^:dynamic main_msub (matrix_sub [[1 2 3] [2 4 5] [6 7 8]] [[1 2 7] [2 4 5] [6 7 10]]))

(def ^:dynamic main_mzero (square_zero_matrix 5))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (assert_int "component0" (vector_component main_vx 0) 1)
      (assert_int "component2" (vector_component main_vx 2) 3)
      (assert_str "str_vector" (vector_str_int main_vs) "(0,0,0,0,0,1)")
      (assert_int "size" (count main_vsize) 4)
      (assert_int "add0" (vector_component main_vsum 0) 2)
      (assert_int "add1" (vector_component main_vsum 1) 3)
      (assert_int "add2" (vector_component main_vsum 2) 4)
      (assert_int "sub0" (vector_component main_vsub 0) 0)
      (assert_int "sub1" (vector_component main_vsub 1) 1)
      (assert_int "sub2" (vector_component main_vsub 2) 2)
      (assert_str "scalar_mul" (vector_str_float main_vmul 1) "(3.0,6.0,9.0)")
      (assert_int "dot_product" (vector_dot [2 (- 1) 4] [1 (- 2) (- 1)]) 0)
      (while (< main_zi (count main_zstr)) (do (when (= (subs main_zstr main_zi (min (+ main_zi 1) (count main_zstr))) "0") (def main_zcount (+ main_zcount 1))) (def main_zi (+ main_zi 1))))
      (assert_int "zero_vector" main_zcount 10)
      (assert_str "unit_basis" (vector_str_int (unit_basis_vector 3 1)) "(0,1,0)")
      (assert_str "axpy" (vector_str_int (axpy 2 [1 2 3] [1 0 1])) "(3,4,7)")
      (assert_str "copy" (vector_str_int main_vcopy) "(1,0,0,0,0,0)")
      (change_component main_vchange 0 0)
      (change_component main_vchange 1 1)
      (assert_str "change_component" (vector_str_int main_vchange) "(0,1,0)")
      (assert_str "matrix_str" (matrix_str main_ma) "|1,2,3|\n|2,4,5|\n|6,7,8|\n")
      (assert_int "determinant" (determinant main_ma) (- 5))
      (assert_str "matrix_vec_mul" (vector_str_int main_mv) "(14,32,50)")
      (assert_str "matrix_scalar_mul" (matrix_str main_msc) "|2,4,6|\n|8,10,12|\n|14,16,18|\n")
      (matrix_change_component main_mc 0 2 5)
      (assert_str "change_component_matrix" (matrix_str main_mc) "|1,2,5|\n|2,4,5|\n|6,7,8|\n")
      (assert_int "matrix_component" (matrix_component main_mc 2 1) 7)
      (assert_str "matrix_add" (matrix_str main_madd) "|2,4,10|\n|4,8,10|\n|12,14,18|\n")
      (assert_str "matrix_sub" (matrix_str main_msub) "|0,0,-4|\n|0,0,0|\n|0,0,-2|\n")
      (assert_str "square_zero_matrix" (matrix_str main_mzero) "|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n")
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
