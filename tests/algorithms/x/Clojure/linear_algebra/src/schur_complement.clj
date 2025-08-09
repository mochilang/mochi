(ns main (:refer-clojure :exclude [identity transpose matmul mat_sub inverse schur_complement print_matrix main]))

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

(declare identity transpose matmul mat_sub inverse schur_complement print_matrix main)

(def ^:dynamic identity_i nil)

(def ^:dynamic identity_j nil)

(def ^:dynamic identity_mat nil)

(def ^:dynamic identity_row nil)

(def ^:dynamic inverse_aug nil)

(def ^:dynamic inverse_c nil)

(def ^:dynamic inverse_col nil)

(def ^:dynamic inverse_factor nil)

(def ^:dynamic inverse_i nil)

(def ^:dynamic inverse_id nil)

(def ^:dynamic inverse_inv nil)

(def ^:dynamic inverse_j nil)

(def ^:dynamic inverse_n nil)

(def ^:dynamic inverse_pivot nil)

(def ^:dynamic inverse_pivot_row nil)

(def ^:dynamic inverse_r nil)

(def ^:dynamic inverse_row nil)

(def ^:dynamic inverse_row_r nil)

(def ^:dynamic main_a nil)

(def ^:dynamic main_b nil)

(def ^:dynamic main_c nil)

(def ^:dynamic main_none nil)

(def ^:dynamic main_s nil)

(def ^:dynamic mat_sub_cols nil)

(def ^:dynamic mat_sub_i nil)

(def ^:dynamic mat_sub_j nil)

(def ^:dynamic mat_sub_res nil)

(def ^:dynamic mat_sub_row nil)

(def ^:dynamic mat_sub_rows nil)

(def ^:dynamic matmul_cols nil)

(def ^:dynamic matmul_i nil)

(def ^:dynamic matmul_inner nil)

(def ^:dynamic matmul_j nil)

(def ^:dynamic matmul_k nil)

(def ^:dynamic matmul_res nil)

(def ^:dynamic matmul_row nil)

(def ^:dynamic matmul_rows nil)

(def ^:dynamic matmul_sum nil)

(def ^:dynamic print_matrix_i nil)

(def ^:dynamic print_matrix_j nil)

(def ^:dynamic print_matrix_line nil)

(def ^:dynamic print_matrix_row nil)

(def ^:dynamic schur_complement_a_cols nil)

(def ^:dynamic schur_complement_a_inv nil)

(def ^:dynamic schur_complement_a_inv_b nil)

(def ^:dynamic schur_complement_a_rows nil)

(def ^:dynamic schur_complement_bt nil)

(def ^:dynamic schur_complement_bt_a_inv_b nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_res nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(defn identity [identity_n]
  (binding [identity_i nil identity_j nil identity_mat nil identity_row nil] (try (do (set! identity_mat []) (set! identity_i 0) (while (< identity_i identity_n) (do (set! identity_row []) (set! identity_j 0) (while (< identity_j identity_n) (do (if (= identity_i identity_j) (set! identity_row (conj identity_row 1.0)) (set! identity_row (conj identity_row 0.0))) (set! identity_j (+ identity_j 1)))) (set! identity_mat (conj identity_mat identity_row)) (set! identity_i (+ identity_i 1)))) (throw (ex-info "return" {:v identity_mat}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_mat]
  (binding [transpose_cols nil transpose_i nil transpose_j nil transpose_res nil transpose_row nil transpose_rows nil] (try (do (set! transpose_rows (count transpose_mat)) (set! transpose_cols (count (nth transpose_mat 0))) (set! transpose_res []) (set! transpose_j 0) (while (< transpose_j transpose_cols) (do (set! transpose_row []) (set! transpose_i 0) (while (< transpose_i transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_mat transpose_i) transpose_j))) (set! transpose_i (+ transpose_i 1)))) (set! transpose_res (conj transpose_res transpose_row)) (set! transpose_j (+ transpose_j 1)))) (throw (ex-info "return" {:v transpose_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matmul [matmul_a matmul_b]
  (binding [matmul_cols nil matmul_i nil matmul_inner nil matmul_j nil matmul_k nil matmul_res nil matmul_row nil matmul_rows nil matmul_sum nil] (try (do (set! matmul_rows (count matmul_a)) (set! matmul_cols (count (nth matmul_b 0))) (set! matmul_inner (count (nth matmul_a 0))) (set! matmul_res []) (set! matmul_i 0) (while (< matmul_i matmul_rows) (do (set! matmul_row []) (set! matmul_j 0) (while (< matmul_j matmul_cols) (do (set! matmul_sum 0.0) (set! matmul_k 0) (while (< matmul_k matmul_inner) (do (set! matmul_sum (+ matmul_sum (* (nth (nth matmul_a matmul_i) matmul_k) (nth (nth matmul_b matmul_k) matmul_j)))) (set! matmul_k (+ matmul_k 1)))) (set! matmul_row (conj matmul_row matmul_sum)) (set! matmul_j (+ matmul_j 1)))) (set! matmul_res (conj matmul_res matmul_row)) (set! matmul_i (+ matmul_i 1)))) (throw (ex-info "return" {:v matmul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_sub [mat_sub_a mat_sub_b]
  (binding [mat_sub_cols nil mat_sub_i nil mat_sub_j nil mat_sub_res nil mat_sub_row nil mat_sub_rows nil] (try (do (set! mat_sub_rows (count mat_sub_a)) (set! mat_sub_cols (count (nth mat_sub_a 0))) (set! mat_sub_res []) (set! mat_sub_i 0) (while (< mat_sub_i mat_sub_rows) (do (set! mat_sub_row []) (set! mat_sub_j 0) (while (< mat_sub_j mat_sub_cols) (do (set! mat_sub_row (conj mat_sub_row (- (nth (nth mat_sub_a mat_sub_i) mat_sub_j) (nth (nth mat_sub_b mat_sub_i) mat_sub_j)))) (set! mat_sub_j (+ mat_sub_j 1)))) (set! mat_sub_res (conj mat_sub_res mat_sub_row)) (set! mat_sub_i (+ mat_sub_i 1)))) (throw (ex-info "return" {:v mat_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inverse [inverse_mat]
  (binding [inverse_aug nil inverse_c nil inverse_col nil inverse_factor nil inverse_i nil inverse_id nil inverse_inv nil inverse_j nil inverse_n nil inverse_pivot nil inverse_pivot_row nil inverse_r nil inverse_row nil inverse_row_r nil] (try (do (set! inverse_n (count inverse_mat)) (set! inverse_id (identity inverse_n)) (set! inverse_aug []) (set! inverse_i 0) (while (< inverse_i inverse_n) (do (set! inverse_row (concat (nth inverse_mat inverse_i) (nth inverse_id inverse_i))) (set! inverse_aug (conj inverse_aug inverse_row)) (set! inverse_i (+ inverse_i 1)))) (set! inverse_col 0) (while (< inverse_col inverse_n) (do (set! inverse_pivot_row (nth inverse_aug inverse_col)) (set! inverse_pivot (nth inverse_pivot_row inverse_col)) (when (= inverse_pivot 0.0) (throw (Exception. "matrix is singular"))) (set! inverse_j 0) (while (< inverse_j (* 2 inverse_n)) (do (set! inverse_pivot_row (assoc inverse_pivot_row inverse_j (quot (nth inverse_pivot_row inverse_j) inverse_pivot))) (set! inverse_j (+ inverse_j 1)))) (set! inverse_aug (assoc inverse_aug inverse_col inverse_pivot_row)) (set! inverse_r 0) (while (< inverse_r inverse_n) (do (when (not= inverse_r inverse_col) (do (set! inverse_row_r (nth inverse_aug inverse_r)) (set! inverse_factor (nth inverse_row_r inverse_col)) (set! inverse_j 0) (while (< inverse_j (* 2 inverse_n)) (do (set! inverse_row_r (assoc inverse_row_r inverse_j (- (nth inverse_row_r inverse_j) (* inverse_factor (nth inverse_pivot_row inverse_j))))) (set! inverse_j (+ inverse_j 1)))) (set! inverse_aug (assoc inverse_aug inverse_r inverse_row_r)))) (set! inverse_r (+ inverse_r 1)))) (set! inverse_col (+ inverse_col 1)))) (set! inverse_inv []) (set! inverse_r 0) (while (< inverse_r inverse_n) (do (set! inverse_row []) (set! inverse_c inverse_n) (while (< inverse_c (* 2 inverse_n)) (do (set! inverse_row (conj inverse_row (nth (nth inverse_aug inverse_r) inverse_c))) (set! inverse_c (+ inverse_c 1)))) (set! inverse_inv (conj inverse_inv inverse_row)) (set! inverse_r (+ inverse_r 1)))) (throw (ex-info "return" {:v inverse_inv}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn schur_complement [schur_complement_mat_a schur_complement_mat_b schur_complement_mat_c schur_complement_pseudo_inv]
  (binding [schur_complement_a_cols nil schur_complement_a_inv nil schur_complement_a_inv_b nil schur_complement_a_rows nil schur_complement_bt nil schur_complement_bt_a_inv_b nil] (try (do (set! schur_complement_a_rows (count schur_complement_mat_a)) (set! schur_complement_a_cols (count (nth schur_complement_mat_a 0))) (when (not= schur_complement_a_rows schur_complement_a_cols) (throw (Exception. "Matrix A must be square"))) (when (not= schur_complement_a_rows (count schur_complement_mat_b)) (throw (Exception. "Expected the same number of rows for A and B"))) (when (not= (count (nth schur_complement_mat_b 0)) (count (nth schur_complement_mat_c 0))) (throw (Exception. "Expected the same number of columns for B and C"))) (set! schur_complement_a_inv []) (if (:ok schur_complement_pseudo_inv) (set! schur_complement_a_inv (:value schur_complement_pseudo_inv)) (set! schur_complement_a_inv (inverse schur_complement_mat_a))) (set! schur_complement_bt (transpose schur_complement_mat_b)) (set! schur_complement_a_inv_b (matmul schur_complement_a_inv schur_complement_mat_b)) (set! schur_complement_bt_a_inv_b (matmul schur_complement_bt schur_complement_a_inv_b)) (throw (ex-info "return" {:v (mat_sub schur_complement_mat_c schur_complement_bt_a_inv_b)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix [print_matrix_mat]
  (binding [print_matrix_i nil print_matrix_j nil print_matrix_line nil print_matrix_row nil] (do (set! print_matrix_i 0) (while (< print_matrix_i (count print_matrix_mat)) (do (set! print_matrix_line "") (set! print_matrix_j 0) (set! print_matrix_row (nth print_matrix_mat print_matrix_i)) (while (< print_matrix_j (count print_matrix_row)) (do (set! print_matrix_line (str print_matrix_line (str (nth print_matrix_row print_matrix_j)))) (when (< (+ print_matrix_j 1) (count print_matrix_row)) (set! print_matrix_line (str print_matrix_line " "))) (set! print_matrix_j (+ print_matrix_j 1)))) (println print_matrix_line) (set! print_matrix_i (+ print_matrix_i 1)))) print_matrix_mat)))

(defn main []
  (binding [main_a nil main_b nil main_c nil main_none nil main_s nil] (do (set! main_a [[1.0 2.0] [2.0 1.0]]) (set! main_b [[0.0 3.0] [3.0 0.0]]) (set! main_c [[2.0 1.0] [6.0 3.0]]) (set! main_none {:ok false :value []}) (set! main_s (schur_complement main_a main_b main_c main_none)) (print_matrix main_s))))

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
