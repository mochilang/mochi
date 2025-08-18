(ns main (:refer-clojure :exclude [make_matrix matrix_columns matrix_identity matrix_minor matrix_cofactor matrix_minors matrix_cofactors matrix_determinant matrix_is_invertible matrix_adjugate matrix_inverse matrix_add_row matrix_add_column matrix_mul_scalar matrix_neg matrix_add matrix_sub matrix_dot matrix_mul matrix_pow matrix_to_string main]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare make_matrix matrix_columns matrix_identity matrix_minor matrix_cofactor matrix_minors matrix_cofactors matrix_determinant matrix_is_invertible matrix_adjugate matrix_inverse matrix_add_row matrix_add_column matrix_mul_scalar matrix_neg matrix_add matrix_sub matrix_dot matrix_mul matrix_pow matrix_to_string main)

(def ^:dynamic main_m nil)

(def ^:dynamic main_m2 nil)

(def ^:dynamic main_m3 nil)

(def ^:dynamic main_m4 nil)

(def ^:dynamic make_matrix_c nil)

(def ^:dynamic make_matrix_i nil)

(def ^:dynamic make_matrix_r nil)

(def ^:dynamic matrix_add_column_i nil)

(def ^:dynamic matrix_add_column_newData nil)

(def ^:dynamic matrix_add_i nil)

(def ^:dynamic matrix_add_j nil)

(def ^:dynamic matrix_add_row_newData nil)

(def ^:dynamic matrix_add_row_v nil)

(def ^:dynamic matrix_add_vals nil)

(def ^:dynamic matrix_adjugate_cof nil)

(def ^:dynamic matrix_adjugate_i nil)

(def ^:dynamic matrix_adjugate_j nil)

(def ^:dynamic matrix_adjugate_row nil)

(def ^:dynamic matrix_adjugate_vals nil)

(def ^:dynamic matrix_cofactor_minor nil)

(def ^:dynamic matrix_cofactors_i nil)

(def ^:dynamic matrix_cofactors_j nil)

(def ^:dynamic matrix_cofactors_row nil)

(def ^:dynamic matrix_cofactors_vals nil)

(def ^:dynamic matrix_columns_col nil)

(def ^:dynamic matrix_columns_cols nil)

(def ^:dynamic matrix_columns_i nil)

(def ^:dynamic matrix_columns_j nil)

(def ^:dynamic matrix_determinant_j nil)

(def ^:dynamic matrix_determinant_sum nil)

(def ^:dynamic matrix_dot_i nil)

(def ^:dynamic matrix_dot_sum nil)

(def ^:dynamic matrix_identity_i nil)

(def ^:dynamic matrix_identity_j nil)

(def ^:dynamic matrix_identity_row nil)

(def ^:dynamic matrix_identity_v nil)

(def ^:dynamic matrix_identity_vals nil)

(def ^:dynamic matrix_inverse_adj nil)

(def ^:dynamic matrix_inverse_det nil)

(def ^:dynamic matrix_minor_i nil)

(def ^:dynamic matrix_minor_j nil)

(def ^:dynamic matrix_minor_row nil)

(def ^:dynamic matrix_minor_sub nil)

(def ^:dynamic matrix_minor_vals nil)

(def ^:dynamic matrix_minors_i nil)

(def ^:dynamic matrix_minors_j nil)

(def ^:dynamic matrix_minors_row nil)

(def ^:dynamic matrix_minors_vals nil)

(def ^:dynamic matrix_mul_bcols nil)

(def ^:dynamic matrix_mul_i nil)

(def ^:dynamic matrix_mul_j nil)

(def ^:dynamic matrix_mul_row nil)

(def ^:dynamic matrix_mul_scalar_i nil)

(def ^:dynamic matrix_mul_scalar_j nil)

(def ^:dynamic matrix_mul_scalar_row nil)

(def ^:dynamic matrix_mul_scalar_vals nil)

(def ^:dynamic matrix_mul_vals nil)

(def ^:dynamic matrix_pow_i nil)

(def ^:dynamic matrix_pow_result nil)

(def ^:dynamic matrix_sub_i nil)

(def ^:dynamic matrix_sub_j nil)

(def ^:dynamic matrix_sub_row nil)

(def ^:dynamic matrix_sub_vals nil)

(def ^:dynamic matrix_to_string_i nil)

(def ^:dynamic matrix_to_string_j nil)

(def ^:dynamic matrix_to_string_s nil)

(defn make_matrix [make_matrix_values]
  (binding [make_matrix_c nil make_matrix_i nil make_matrix_r nil] (try (do (set! make_matrix_r (count make_matrix_values)) (when (= make_matrix_r 0) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! make_matrix_c (count (nth make_matrix_values 0))) (set! make_matrix_i 0) (while (< make_matrix_i make_matrix_r) (do (when (not= (count (nth make_matrix_values make_matrix_i)) make_matrix_c) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! make_matrix_i (+ make_matrix_i 1)))) (throw (ex-info "return" {:v {:cols make_matrix_c :data make_matrix_values :rows make_matrix_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_columns [matrix_columns_m]
  (binding [matrix_columns_col nil matrix_columns_cols nil matrix_columns_i nil matrix_columns_j nil] (try (do (set! matrix_columns_cols []) (set! matrix_columns_j 0) (while (< matrix_columns_j (:cols matrix_columns_m)) (do (set! matrix_columns_col []) (set! matrix_columns_i 0) (while (< matrix_columns_i (:rows matrix_columns_m)) (do (set! matrix_columns_col (conj matrix_columns_col (get (get (:data matrix_columns_m) matrix_columns_i) matrix_columns_j))) (set! matrix_columns_i (+ matrix_columns_i 1)))) (set! matrix_columns_cols (conj matrix_columns_cols matrix_columns_col)) (set! matrix_columns_j (+ matrix_columns_j 1)))) (throw (ex-info "return" {:v matrix_columns_cols}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_identity [matrix_identity_m]
  (binding [matrix_identity_i nil matrix_identity_j nil matrix_identity_row nil matrix_identity_v nil matrix_identity_vals nil] (try (do (set! matrix_identity_vals []) (set! matrix_identity_i 0) (while (< matrix_identity_i (:rows matrix_identity_m)) (do (set! matrix_identity_row []) (set! matrix_identity_j 0) (while (< matrix_identity_j (:cols matrix_identity_m)) (do (set! matrix_identity_v (if (= matrix_identity_i matrix_identity_j) 1.0 0.0)) (set! matrix_identity_row (conj matrix_identity_row matrix_identity_v)) (set! matrix_identity_j (+ matrix_identity_j 1)))) (set! matrix_identity_vals (conj matrix_identity_vals matrix_identity_row)) (set! matrix_identity_i (+ matrix_identity_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_identity_m) :data matrix_identity_vals :rows (:rows matrix_identity_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_minor [matrix_minor_m matrix_minor_r matrix_minor_c]
  (binding [matrix_minor_i nil matrix_minor_j nil matrix_minor_row nil matrix_minor_sub nil matrix_minor_vals nil] (try (do (set! matrix_minor_vals []) (set! matrix_minor_i 0) (while (< matrix_minor_i (:rows matrix_minor_m)) (do (when (not= matrix_minor_i matrix_minor_r) (do (set! matrix_minor_row []) (set! matrix_minor_j 0) (while (< matrix_minor_j (:cols matrix_minor_m)) (do (when (not= matrix_minor_j matrix_minor_c) (set! matrix_minor_row (conj matrix_minor_row (get (get (:data matrix_minor_m) matrix_minor_i) matrix_minor_j)))) (set! matrix_minor_j (+ matrix_minor_j 1)))) (set! matrix_minor_vals (conj matrix_minor_vals matrix_minor_row)))) (set! matrix_minor_i (+ matrix_minor_i 1)))) (set! matrix_minor_sub {:cols (- (:cols matrix_minor_m) 1) :data matrix_minor_vals :rows (- (:rows matrix_minor_m) 1)}) (throw (ex-info "return" {:v (matrix_determinant matrix_minor_sub)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_cofactor [matrix_cofactor_m matrix_cofactor_r matrix_cofactor_c]
  (binding [matrix_cofactor_minor nil] (try (do (set! matrix_cofactor_minor (matrix_minor matrix_cofactor_m matrix_cofactor_r matrix_cofactor_c)) (if (= (mod (+ matrix_cofactor_r matrix_cofactor_c) 2) 0) matrix_cofactor_minor (* (- 1.0) matrix_cofactor_minor))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_minors [matrix_minors_m]
  (binding [matrix_minors_i nil matrix_minors_j nil matrix_minors_row nil matrix_minors_vals nil] (try (do (set! matrix_minors_vals []) (set! matrix_minors_i 0) (while (< matrix_minors_i (:rows matrix_minors_m)) (do (set! matrix_minors_row []) (set! matrix_minors_j 0) (while (< matrix_minors_j (:cols matrix_minors_m)) (do (set! matrix_minors_row (conj matrix_minors_row (matrix_minor matrix_minors_m matrix_minors_i matrix_minors_j))) (set! matrix_minors_j (+ matrix_minors_j 1)))) (set! matrix_minors_vals (conj matrix_minors_vals matrix_minors_row)) (set! matrix_minors_i (+ matrix_minors_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_minors_m) :data matrix_minors_vals :rows (:rows matrix_minors_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_cofactors [matrix_cofactors_m]
  (binding [matrix_cofactors_i nil matrix_cofactors_j nil matrix_cofactors_row nil matrix_cofactors_vals nil] (try (do (set! matrix_cofactors_vals []) (set! matrix_cofactors_i 0) (while (< matrix_cofactors_i (:rows matrix_cofactors_m)) (do (set! matrix_cofactors_row []) (set! matrix_cofactors_j 0) (while (< matrix_cofactors_j (:cols matrix_cofactors_m)) (do (set! matrix_cofactors_row (conj matrix_cofactors_row (matrix_cofactor matrix_cofactors_m matrix_cofactors_i matrix_cofactors_j))) (set! matrix_cofactors_j (+ matrix_cofactors_j 1)))) (set! matrix_cofactors_vals (conj matrix_cofactors_vals matrix_cofactors_row)) (set! matrix_cofactors_i (+ matrix_cofactors_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_cofactors_m) :data matrix_cofactors_vals :rows (:rows matrix_cofactors_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_determinant [matrix_determinant_m]
  (binding [matrix_determinant_j nil matrix_determinant_sum nil] (try (do (when (not= (:rows matrix_determinant_m) (:cols matrix_determinant_m)) (throw (ex-info "return" {:v 0.0}))) (when (= (:rows matrix_determinant_m) 0) (throw (ex-info "return" {:v 0.0}))) (when (= (:rows matrix_determinant_m) 1) (throw (ex-info "return" {:v (get (get (:data matrix_determinant_m) 0) 0)}))) (when (= (:rows matrix_determinant_m) 2) (throw (ex-info "return" {:v (- (* (get (get (:data matrix_determinant_m) 0) 0) (get (get (:data matrix_determinant_m) 1) 1)) (* (get (get (:data matrix_determinant_m) 0) 1) (get (get (:data matrix_determinant_m) 1) 0)))}))) (set! matrix_determinant_sum 0.0) (set! matrix_determinant_j 0) (while (< matrix_determinant_j (:cols matrix_determinant_m)) (do (set! matrix_determinant_sum (+ matrix_determinant_sum (* (get (get (:data matrix_determinant_m) 0) matrix_determinant_j) (matrix_cofactor matrix_determinant_m 0 matrix_determinant_j)))) (set! matrix_determinant_j (+ matrix_determinant_j 1)))) (throw (ex-info "return" {:v matrix_determinant_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_is_invertible [matrix_is_invertible_m]
  (try (throw (ex-info "return" {:v (not= (matrix_determinant matrix_is_invertible_m) 0.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn matrix_adjugate [matrix_adjugate_m]
  (binding [matrix_adjugate_cof nil matrix_adjugate_i nil matrix_adjugate_j nil matrix_adjugate_row nil matrix_adjugate_vals nil] (try (do (set! matrix_adjugate_cof (matrix_cofactors matrix_adjugate_m)) (set! matrix_adjugate_vals []) (set! matrix_adjugate_i 0) (while (< matrix_adjugate_i (:rows matrix_adjugate_m)) (do (set! matrix_adjugate_row []) (set! matrix_adjugate_j 0) (while (< matrix_adjugate_j (:cols matrix_adjugate_m)) (do (set! matrix_adjugate_row (conj matrix_adjugate_row (get (get (:data matrix_adjugate_cof) matrix_adjugate_j) matrix_adjugate_i))) (set! matrix_adjugate_j (+ matrix_adjugate_j 1)))) (set! matrix_adjugate_vals (conj matrix_adjugate_vals matrix_adjugate_row)) (set! matrix_adjugate_i (+ matrix_adjugate_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_adjugate_m) :data matrix_adjugate_vals :rows (:rows matrix_adjugate_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_inverse [matrix_inverse_m]
  (binding [matrix_inverse_adj nil matrix_inverse_det nil] (try (do (set! matrix_inverse_det (matrix_determinant matrix_inverse_m)) (when (= matrix_inverse_det 0.0) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! matrix_inverse_adj (matrix_adjugate matrix_inverse_m)) (throw (ex-info "return" {:v (matrix_mul_scalar matrix_inverse_adj (/ 1.0 matrix_inverse_det))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_add_row [matrix_add_row_m matrix_add_row_row]
  (binding [matrix_add_row_newData nil] (try (do (set! matrix_add_row_newData (:data matrix_add_row_m)) (set! matrix_add_row_newData (conj matrix_add_row_newData matrix_add_row_row)) (throw (ex-info "return" {:v {:cols (:cols matrix_add_row_m) :data matrix_add_row_newData :rows (+ (:rows matrix_add_row_m) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_add_column [matrix_add_column_m matrix_add_column_col]
  (binding [matrix_add_column_i nil matrix_add_column_newData nil] (try (do (set! matrix_add_column_newData []) (set! matrix_add_column_i 0) (while (< matrix_add_column_i (:rows matrix_add_column_m)) (do (set! matrix_add_column_newData (conj matrix_add_column_newData (conj (get (:data matrix_add_column_m) matrix_add_column_i) (nth matrix_add_column_col matrix_add_column_i)))) (set! matrix_add_column_i (+ matrix_add_column_i 1)))) (throw (ex-info "return" {:v {:cols (+ (:cols matrix_add_column_m) 1) :data matrix_add_column_newData :rows (:rows matrix_add_column_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul_scalar [matrix_mul_scalar_m matrix_mul_scalar_s]
  (binding [matrix_mul_scalar_i nil matrix_mul_scalar_j nil matrix_mul_scalar_row nil matrix_mul_scalar_vals nil] (try (do (set! matrix_mul_scalar_vals []) (set! matrix_mul_scalar_i 0) (while (< matrix_mul_scalar_i (:rows matrix_mul_scalar_m)) (do (set! matrix_mul_scalar_row []) (set! matrix_mul_scalar_j 0) (while (< matrix_mul_scalar_j (:cols matrix_mul_scalar_m)) (do (set! matrix_mul_scalar_row (conj matrix_mul_scalar_row (* (get (get (:data matrix_mul_scalar_m) matrix_mul_scalar_i) matrix_mul_scalar_j) matrix_mul_scalar_s))) (set! matrix_mul_scalar_j (+ matrix_mul_scalar_j 1)))) (set! matrix_mul_scalar_vals (conj matrix_mul_scalar_vals matrix_mul_scalar_row)) (set! matrix_mul_scalar_i (+ matrix_mul_scalar_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_mul_scalar_m) :data matrix_mul_scalar_vals :rows (:rows matrix_mul_scalar_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_neg [matrix_neg_m]
  (try (throw (ex-info "return" {:v (matrix_mul_scalar matrix_neg_m (- 1.0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn matrix_add [matrix_add_a matrix_add_b]
  (binding [matrix_add_i nil matrix_add_j nil matrix_add_row_v nil matrix_add_vals nil] (try (do (when (or (not= (:rows matrix_add_a) (:rows matrix_add_b)) (not= (:cols matrix_add_a) (:cols matrix_add_b))) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! matrix_add_vals []) (set! matrix_add_i 0) (while (< matrix_add_i (:rows matrix_add_a)) (do (set! matrix_add_row_v []) (set! matrix_add_j 0) (while (< matrix_add_j (:cols matrix_add_a)) (do (set! matrix_add_row_v (conj matrix_add_row_v (+ (get (get (:data matrix_add_a) matrix_add_i) matrix_add_j) (get (get (:data matrix_add_b) matrix_add_i) matrix_add_j)))) (set! matrix_add_j (+ matrix_add_j 1)))) (set! matrix_add_vals (conj matrix_add_vals matrix_add_row_v)) (set! matrix_add_i (+ matrix_add_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_add_a) :data matrix_add_vals :rows (:rows matrix_add_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_sub [matrix_sub_a matrix_sub_b]
  (binding [matrix_sub_i nil matrix_sub_j nil matrix_sub_row nil matrix_sub_vals nil] (try (do (when (or (not= (:rows matrix_sub_a) (:rows matrix_sub_b)) (not= (:cols matrix_sub_a) (:cols matrix_sub_b))) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! matrix_sub_vals []) (set! matrix_sub_i 0) (while (< matrix_sub_i (:rows matrix_sub_a)) (do (set! matrix_sub_row []) (set! matrix_sub_j 0) (while (< matrix_sub_j (:cols matrix_sub_a)) (do (set! matrix_sub_row (conj matrix_sub_row (- (get (get (:data matrix_sub_a) matrix_sub_i) matrix_sub_j) (get (get (:data matrix_sub_b) matrix_sub_i) matrix_sub_j)))) (set! matrix_sub_j (+ matrix_sub_j 1)))) (set! matrix_sub_vals (conj matrix_sub_vals matrix_sub_row)) (set! matrix_sub_i (+ matrix_sub_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_sub_a) :data matrix_sub_vals :rows (:rows matrix_sub_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_dot [matrix_dot_row matrix_dot_col]
  (binding [matrix_dot_i nil matrix_dot_sum nil] (try (do (set! matrix_dot_sum 0.0) (set! matrix_dot_i 0) (while (< matrix_dot_i (count matrix_dot_row)) (do (set! matrix_dot_sum (+ matrix_dot_sum (* (nth matrix_dot_row matrix_dot_i) (nth matrix_dot_col matrix_dot_i)))) (set! matrix_dot_i (+ matrix_dot_i 1)))) (throw (ex-info "return" {:v matrix_dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul [matrix_mul_a matrix_mul_b]
  (binding [matrix_mul_bcols nil matrix_mul_i nil matrix_mul_j nil matrix_mul_row nil matrix_mul_vals nil] (try (do (when (not= (:cols matrix_mul_a) (:rows matrix_mul_b)) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! matrix_mul_bcols (matrix_columns matrix_mul_b)) (set! matrix_mul_vals []) (set! matrix_mul_i 0) (while (< matrix_mul_i (:rows matrix_mul_a)) (do (set! matrix_mul_row []) (set! matrix_mul_j 0) (while (< matrix_mul_j (:cols matrix_mul_b)) (do (set! matrix_mul_row (conj matrix_mul_row (matrix_dot (get (:data matrix_mul_a) matrix_mul_i) (nth matrix_mul_bcols matrix_mul_j)))) (set! matrix_mul_j (+ matrix_mul_j 1)))) (set! matrix_mul_vals (conj matrix_mul_vals matrix_mul_row)) (set! matrix_mul_i (+ matrix_mul_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_mul_b) :data matrix_mul_vals :rows (:rows matrix_mul_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_pow [matrix_pow_m matrix_pow_p]
  (binding [matrix_pow_i nil matrix_pow_result nil] (try (do (when (= matrix_pow_p 0) (throw (ex-info "return" {:v (matrix_identity matrix_pow_m)}))) (when (< matrix_pow_p 0) (do (when (matrix_is_invertible matrix_pow_m) (throw (ex-info "return" {:v (matrix_pow (matrix_inverse matrix_pow_m) (- matrix_pow_p))}))) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}})))) (set! matrix_pow_result matrix_pow_m) (set! matrix_pow_i 1) (while (< matrix_pow_i matrix_pow_p) (do (set! matrix_pow_result (matrix_mul matrix_pow_result matrix_pow_m)) (set! matrix_pow_i (+ matrix_pow_i 1)))) (throw (ex-info "return" {:v matrix_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_to_string [matrix_to_string_m]
  (binding [matrix_to_string_i nil matrix_to_string_j nil matrix_to_string_s nil] (try (do (when (= (:rows matrix_to_string_m) 0) (throw (ex-info "return" {:v "[]"}))) (set! matrix_to_string_s "[") (set! matrix_to_string_i 0) (while (< matrix_to_string_i (:rows matrix_to_string_m)) (do (set! matrix_to_string_s (str matrix_to_string_s "[")) (set! matrix_to_string_j 0) (while (< matrix_to_string_j (:cols matrix_to_string_m)) (do (set! matrix_to_string_s (str matrix_to_string_s (mochi_str (get (get (:data matrix_to_string_m) matrix_to_string_i) matrix_to_string_j)))) (when (< matrix_to_string_j (- (:cols matrix_to_string_m) 1)) (set! matrix_to_string_s (str matrix_to_string_s " "))) (set! matrix_to_string_j (+ matrix_to_string_j 1)))) (set! matrix_to_string_s (str matrix_to_string_s "]")) (when (< matrix_to_string_i (- (:rows matrix_to_string_m) 1)) (set! matrix_to_string_s (str matrix_to_string_s "\n "))) (set! matrix_to_string_i (+ matrix_to_string_i 1)))) (set! matrix_to_string_s (str matrix_to_string_s "]")) (throw (ex-info "return" {:v matrix_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_m nil main_m2 nil main_m3 nil main_m4 nil] (do (set! main_m (make_matrix [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]])) (println (matrix_to_string main_m)) (println (mochi_str (matrix_columns main_m))) (println (str (str (mochi_str (:rows main_m)) ",") (mochi_str (:cols main_m)))) (println (mochi_str (matrix_is_invertible main_m))) (println (matrix_to_string (matrix_identity main_m))) (println (mochi_str (matrix_determinant main_m))) (println (matrix_to_string (matrix_minors main_m))) (println (matrix_to_string (matrix_cofactors main_m))) (println (matrix_to_string (matrix_adjugate main_m))) (set! main_m2 (matrix_mul_scalar main_m 3.0)) (println (matrix_to_string main_m2)) (println (matrix_to_string (matrix_add main_m main_m2))) (println (matrix_to_string (matrix_sub main_m main_m2))) (println (matrix_to_string (matrix_pow main_m 3))) (set! main_m3 (matrix_add_row main_m [10.0 11.0 12.0])) (println (matrix_to_string main_m3)) (set! main_m4 (matrix_add_column main_m2 [8.0 16.0 32.0])) (println (matrix_to_string (matrix_mul main_m3 main_m4))))))

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
