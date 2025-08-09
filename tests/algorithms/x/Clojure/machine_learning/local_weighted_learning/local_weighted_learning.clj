(ns main (:refer-clojure :exclude [expApprox transpose matMul matInv weight_matrix local_weight local_weight_regression]))

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

(declare expApprox transpose matMul matInv weight_matrix local_weight local_weight_regression)

(def ^:dynamic expApprox_half nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic local_weight_i nil)

(def ^:dynamic local_weight_inv_part nil)

(def ^:dynamic local_weight_regression_i nil)

(def ^:dynamic local_weight_regression_j nil)

(def ^:dynamic local_weight_regression_k nil)

(def ^:dynamic local_weight_regression_m nil)

(def ^:dynamic local_weight_regression_pred nil)

(def ^:dynamic local_weight_regression_preds nil)

(def ^:dynamic local_weight_regression_theta nil)

(def ^:dynamic local_weight_regression_weights_vec nil)

(def ^:dynamic local_weight_w nil)

(def ^:dynamic local_weight_x_t nil)

(def ^:dynamic local_weight_x_t_w nil)

(def ^:dynamic local_weight_x_t_w_x nil)

(def ^:dynamic local_weight_x_t_w_y nil)

(def ^:dynamic local_weight_y_col nil)

(def ^:dynamic matInv_aug nil)

(def ^:dynamic matInv_col nil)

(def ^:dynamic matInv_factor nil)

(def ^:dynamic matInv_i nil)

(def ^:dynamic matInv_inv nil)

(def ^:dynamic matInv_j nil)

(def ^:dynamic matInv_n nil)

(def ^:dynamic matInv_pivot nil)

(def ^:dynamic matInv_r nil)

(def ^:dynamic matInv_row nil)

(def ^:dynamic matMul_a_cols nil)

(def ^:dynamic matMul_a_rows nil)

(def ^:dynamic matMul_b_cols nil)

(def ^:dynamic matMul_i nil)

(def ^:dynamic matMul_j nil)

(def ^:dynamic matMul_k nil)

(def ^:dynamic matMul_res nil)

(def ^:dynamic matMul_row nil)

(def ^:dynamic matMul_sum nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_res nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(def ^:dynamic weight_matrix_diff nil)

(def ^:dynamic weight_matrix_diff_sq nil)

(def ^:dynamic weight_matrix_i nil)

(def ^:dynamic weight_matrix_j nil)

(def ^:dynamic weight_matrix_k nil)

(def ^:dynamic weight_matrix_m nil)

(def ^:dynamic weight_matrix_row nil)

(def ^:dynamic weight_matrix_weights nil)

(defn expApprox [expApprox_x]
  (binding [expApprox_half nil expApprox_n nil expApprox_sum nil expApprox_term nil] (try (do (when (< expApprox_x 0.0) (throw (ex-info "return" {:v (/ 1.0 (expApprox (- expApprox_x)))}))) (when (> expApprox_x 1.0) (do (set! expApprox_half (expApprox (/ expApprox_x 2.0))) (throw (ex-info "return" {:v (* expApprox_half expApprox_half)})))) (set! expApprox_sum 1.0) (set! expApprox_term 1.0) (set! expApprox_n 1) (while (< expApprox_n 20) (do (set! expApprox_term (quot (* expApprox_term expApprox_x) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (throw (ex-info "return" {:v expApprox_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_mat]
  (binding [transpose_cols nil transpose_i nil transpose_j nil transpose_res nil transpose_row nil transpose_rows nil] (try (do (set! transpose_rows (count transpose_mat)) (set! transpose_cols (count (nth transpose_mat 0))) (set! transpose_res []) (set! transpose_i 0) (while (< transpose_i transpose_cols) (do (set! transpose_row []) (set! transpose_j 0) (while (< transpose_j transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_mat transpose_j) transpose_i))) (set! transpose_j (+ transpose_j 1)))) (set! transpose_res (conj transpose_res transpose_row)) (set! transpose_i (+ transpose_i 1)))) (throw (ex-info "return" {:v transpose_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matMul [matMul_a matMul_b]
  (binding [matMul_a_cols nil matMul_a_rows nil matMul_b_cols nil matMul_i nil matMul_j nil matMul_k nil matMul_res nil matMul_row nil matMul_sum nil] (try (do (set! matMul_a_rows (count matMul_a)) (set! matMul_a_cols (count (nth matMul_a 0))) (set! matMul_b_cols (count (nth matMul_b 0))) (set! matMul_res []) (set! matMul_i 0) (while (< matMul_i matMul_a_rows) (do (set! matMul_row []) (set! matMul_j 0) (while (< matMul_j matMul_b_cols) (do (set! matMul_sum 0.0) (set! matMul_k 0) (while (< matMul_k matMul_a_cols) (do (set! matMul_sum (+ matMul_sum (* (nth (nth matMul_a matMul_i) matMul_k) (nth (nth matMul_b matMul_k) matMul_j)))) (set! matMul_k (+ matMul_k 1)))) (set! matMul_row (conj matMul_row matMul_sum)) (set! matMul_j (+ matMul_j 1)))) (set! matMul_res (conj matMul_res matMul_row)) (set! matMul_i (+ matMul_i 1)))) (throw (ex-info "return" {:v matMul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matInv [matInv_mat]
  (binding [matInv_aug nil matInv_col nil matInv_factor nil matInv_i nil matInv_inv nil matInv_j nil matInv_n nil matInv_pivot nil matInv_r nil matInv_row nil] (try (do (set! matInv_n (count matInv_mat)) (set! matInv_aug []) (set! matInv_i 0) (while (< matInv_i matInv_n) (do (set! matInv_row []) (set! matInv_j 0) (while (< matInv_j matInv_n) (do (set! matInv_row (conj matInv_row (nth (nth matInv_mat matInv_i) matInv_j))) (set! matInv_j (+ matInv_j 1)))) (set! matInv_j 0) (while (< matInv_j matInv_n) (do (if (= matInv_i matInv_j) (set! matInv_row (conj matInv_row 1.0)) (set! matInv_row (conj matInv_row 0.0))) (set! matInv_j (+ matInv_j 1)))) (set! matInv_aug (conj matInv_aug matInv_row)) (set! matInv_i (+ matInv_i 1)))) (set! matInv_col 0) (while (< matInv_col matInv_n) (do (set! matInv_pivot (nth (nth matInv_aug matInv_col) matInv_col)) (when (= matInv_pivot 0.0) (throw (Exception. "Matrix is singular"))) (set! matInv_j 0) (while (< matInv_j (* 2 matInv_n)) (do (set! matInv_aug (assoc-in matInv_aug [matInv_col matInv_j] (quot (nth (nth matInv_aug matInv_col) matInv_j) matInv_pivot))) (set! matInv_j (+ matInv_j 1)))) (set! matInv_r 0) (while (< matInv_r matInv_n) (do (when (not= matInv_r matInv_col) (do (set! matInv_factor (nth (nth matInv_aug matInv_r) matInv_col)) (set! matInv_j 0) (while (< matInv_j (* 2 matInv_n)) (do (set! matInv_aug (assoc-in matInv_aug [matInv_r matInv_j] (- (nth (nth matInv_aug matInv_r) matInv_j) (* matInv_factor (nth (nth matInv_aug matInv_col) matInv_j))))) (set! matInv_j (+ matInv_j 1)))))) (set! matInv_r (+ matInv_r 1)))) (set! matInv_col (+ matInv_col 1)))) (set! matInv_inv []) (set! matInv_i 0) (while (< matInv_i matInv_n) (do (set! matInv_row []) (set! matInv_j 0) (while (< matInv_j matInv_n) (do (set! matInv_row (conj matInv_row (nth (nth matInv_aug matInv_i) (+ matInv_j matInv_n)))) (set! matInv_j (+ matInv_j 1)))) (set! matInv_inv (conj matInv_inv matInv_row)) (set! matInv_i (+ matInv_i 1)))) (throw (ex-info "return" {:v matInv_inv}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn weight_matrix [weight_matrix_point weight_matrix_x_train weight_matrix_tau]
  (binding [weight_matrix_diff nil weight_matrix_diff_sq nil weight_matrix_i nil weight_matrix_j nil weight_matrix_k nil weight_matrix_m nil weight_matrix_row nil weight_matrix_weights nil] (try (do (set! weight_matrix_m (count weight_matrix_x_train)) (set! weight_matrix_weights []) (set! weight_matrix_i 0) (while (< weight_matrix_i weight_matrix_m) (do (set! weight_matrix_row []) (set! weight_matrix_j 0) (while (< weight_matrix_j weight_matrix_m) (do (if (= weight_matrix_i weight_matrix_j) (set! weight_matrix_row (conj weight_matrix_row 1.0)) (set! weight_matrix_row (conj weight_matrix_row 0.0))) (set! weight_matrix_j (+ weight_matrix_j 1)))) (set! weight_matrix_weights (conj weight_matrix_weights weight_matrix_row)) (set! weight_matrix_i (+ weight_matrix_i 1)))) (set! weight_matrix_j 0) (while (< weight_matrix_j weight_matrix_m) (do (set! weight_matrix_diff_sq 0.0) (set! weight_matrix_k 0) (while (< weight_matrix_k (count weight_matrix_point)) (do (set! weight_matrix_diff (- (nth weight_matrix_point weight_matrix_k) (nth (nth weight_matrix_x_train weight_matrix_j) weight_matrix_k))) (set! weight_matrix_diff_sq (+ weight_matrix_diff_sq (* weight_matrix_diff weight_matrix_diff))) (set! weight_matrix_k (+ weight_matrix_k 1)))) (set! weight_matrix_weights (assoc-in weight_matrix_weights [weight_matrix_j weight_matrix_j] (expApprox (quot (- weight_matrix_diff_sq) (* (* 2.0 weight_matrix_tau) weight_matrix_tau))))) (set! weight_matrix_j (+ weight_matrix_j 1)))) (throw (ex-info "return" {:v weight_matrix_weights}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn local_weight [local_weight_point local_weight_x_train local_weight_y_train local_weight_tau]
  (binding [local_weight_i nil local_weight_inv_part nil local_weight_w nil local_weight_x_t nil local_weight_x_t_w nil local_weight_x_t_w_x nil local_weight_x_t_w_y nil local_weight_y_col nil] (try (do (set! local_weight_w (weight_matrix local_weight_point local_weight_x_train local_weight_tau)) (set! local_weight_x_t (transpose local_weight_x_train)) (set! local_weight_x_t_w (matMul local_weight_x_t local_weight_w)) (set! local_weight_x_t_w_x (matMul local_weight_x_t_w local_weight_x_train)) (set! local_weight_inv_part (matInv local_weight_x_t_w_x)) (set! local_weight_y_col []) (set! local_weight_i 0) (while (< local_weight_i (count local_weight_y_train)) (do (set! local_weight_y_col (conj local_weight_y_col [(nth local_weight_y_train local_weight_i)])) (set! local_weight_i (+ local_weight_i 1)))) (set! local_weight_x_t_w_y (matMul local_weight_x_t_w local_weight_y_col)) (throw (ex-info "return" {:v (matMul local_weight_inv_part local_weight_x_t_w_y)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn local_weight_regression [local_weight_regression_x_train local_weight_regression_y_train local_weight_regression_tau]
  (binding [local_weight_regression_i nil local_weight_regression_j nil local_weight_regression_k nil local_weight_regression_m nil local_weight_regression_pred nil local_weight_regression_preds nil local_weight_regression_theta nil local_weight_regression_weights_vec nil] (try (do (set! local_weight_regression_m (count local_weight_regression_x_train)) (set! local_weight_regression_preds []) (set! local_weight_regression_i 0) (while (< local_weight_regression_i local_weight_regression_m) (do (set! local_weight_regression_theta (local_weight (nth local_weight_regression_x_train local_weight_regression_i) local_weight_regression_x_train local_weight_regression_y_train local_weight_regression_tau)) (set! local_weight_regression_weights_vec []) (set! local_weight_regression_k 0) (while (< local_weight_regression_k (count local_weight_regression_theta)) (do (set! local_weight_regression_weights_vec (conj local_weight_regression_weights_vec (nth (nth local_weight_regression_theta local_weight_regression_k) 0))) (set! local_weight_regression_k (+ local_weight_regression_k 1)))) (set! local_weight_regression_pred 0.0) (set! local_weight_regression_j 0) (while (< local_weight_regression_j (count (nth local_weight_regression_x_train local_weight_regression_i))) (do (set! local_weight_regression_pred (+ local_weight_regression_pred (* (nth (nth local_weight_regression_x_train local_weight_regression_i) local_weight_regression_j) (nth local_weight_regression_weights_vec local_weight_regression_j)))) (set! local_weight_regression_j (+ local_weight_regression_j 1)))) (set! local_weight_regression_preds (conj local_weight_regression_preds local_weight_regression_pred)) (set! local_weight_regression_i (+ local_weight_regression_i 1)))) (throw (ex-info "return" {:v local_weight_regression_preds}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_x_train [[16.99 10.34] [21.01 23.68] [24.59 25.69]])

(def ^:dynamic main_y_train [1.01 1.66 3.5])

(def ^:dynamic main_preds (local_weight_regression main_x_train main_y_train 0.6))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) main_preds))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
