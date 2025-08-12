(ns main (:refer-clojure :exclude [add subtract scalar_multiply multiply identity transpose minor determinant inverse main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare add subtract scalar_multiply multiply identity transpose minor determinant inverse main)

(def ^:dynamic add_c nil)

(def ^:dynamic add_cols nil)

(def ^:dynamic add_m nil)

(def ^:dynamic add_r nil)

(def ^:dynamic add_result nil)

(def ^:dynamic add_row nil)

(def ^:dynamic add_rows nil)

(def ^:dynamic add_sum nil)

(def ^:dynamic determinant_c nil)

(def ^:dynamic determinant_det nil)

(def ^:dynamic determinant_sign nil)

(def ^:dynamic determinant_sub nil)

(def ^:dynamic identity_i nil)

(def ^:dynamic identity_j nil)

(def ^:dynamic identity_result nil)

(def ^:dynamic identity_row nil)

(def ^:dynamic inverse_adjugate nil)

(def ^:dynamic inverse_cofactors nil)

(def ^:dynamic inverse_det nil)

(def ^:dynamic inverse_i nil)

(def ^:dynamic inverse_j nil)

(def ^:dynamic inverse_m nil)

(def ^:dynamic inverse_matrix_minor nil)

(def ^:dynamic inverse_row nil)

(def ^:dynamic inverse_sign nil)

(def ^:dynamic inverse_size nil)

(def ^:dynamic main_matrix_a nil)

(def ^:dynamic main_matrix_b nil)

(def ^:dynamic main_matrix_c nil)

(def ^:dynamic main_matrix_d nil)

(def ^:dynamic minor_i nil)

(def ^:dynamic minor_j nil)

(def ^:dynamic minor_new_row nil)

(def ^:dynamic minor_result nil)

(def ^:dynamic multiply_colsA nil)

(def ^:dynamic multiply_colsB nil)

(def ^:dynamic multiply_i nil)

(def ^:dynamic multiply_j nil)

(def ^:dynamic multiply_k nil)

(def ^:dynamic multiply_result nil)

(def ^:dynamic multiply_row nil)

(def ^:dynamic multiply_rowsA nil)

(def ^:dynamic multiply_rowsB nil)

(def ^:dynamic multiply_sum nil)

(def ^:dynamic scalar_multiply_i nil)

(def ^:dynamic scalar_multiply_j nil)

(def ^:dynamic scalar_multiply_result nil)

(def ^:dynamic scalar_multiply_row nil)

(def ^:dynamic subtract_c nil)

(def ^:dynamic subtract_cols nil)

(def ^:dynamic subtract_r nil)

(def ^:dynamic subtract_result nil)

(def ^:dynamic subtract_row nil)

(def ^:dynamic subtract_rows nil)

(def ^:dynamic transpose_c nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_r nil)

(def ^:dynamic transpose_result nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(defn add [add_matrices]
  (binding [add_c nil add_cols nil add_m nil add_r nil add_result nil add_row nil add_rows nil add_sum nil] (try (do (set! add_rows (count (nth add_matrices 0))) (set! add_cols (count (nth (nth add_matrices 0) 0))) (set! add_r 0) (set! add_result []) (while (< add_r add_rows) (do (set! add_row []) (set! add_c 0) (while (< add_c add_cols) (do (set! add_sum 0.0) (set! add_m 0) (while (< add_m (count add_matrices)) (do (set! add_sum (+ add_sum (nth (nth (nth add_matrices add_m) add_r) add_c))) (set! add_m (+ add_m 1)))) (set! add_row (conj add_row add_sum)) (set! add_c (+ add_c 1)))) (set! add_result (conj add_result add_row)) (set! add_r (+ add_r 1)))) (throw (ex-info "return" {:v add_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn subtract [subtract_a subtract_b]
  (binding [subtract_c nil subtract_cols nil subtract_r nil subtract_result nil subtract_row nil subtract_rows nil] (try (do (set! subtract_rows (count subtract_a)) (set! subtract_cols (count (nth subtract_a 0))) (set! subtract_r 0) (set! subtract_result []) (while (< subtract_r subtract_rows) (do (set! subtract_row []) (set! subtract_c 0) (while (< subtract_c subtract_cols) (do (set! subtract_row (conj subtract_row (- (nth (nth subtract_a subtract_r) subtract_c) (nth (nth subtract_b subtract_r) subtract_c)))) (set! subtract_c (+ subtract_c 1)))) (set! subtract_result (conj subtract_result subtract_row)) (set! subtract_r (+ subtract_r 1)))) (throw (ex-info "return" {:v subtract_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn scalar_multiply [scalar_multiply_matrix scalar_multiply_n]
  (binding [scalar_multiply_i nil scalar_multiply_j nil scalar_multiply_result nil scalar_multiply_row nil] (try (do (set! scalar_multiply_result []) (set! scalar_multiply_i 0) (while (< scalar_multiply_i (count scalar_multiply_matrix)) (do (set! scalar_multiply_row []) (set! scalar_multiply_j 0) (while (< scalar_multiply_j (count (nth scalar_multiply_matrix scalar_multiply_i))) (do (set! scalar_multiply_row (conj scalar_multiply_row (* (nth (nth scalar_multiply_matrix scalar_multiply_i) scalar_multiply_j) scalar_multiply_n))) (set! scalar_multiply_j (+ scalar_multiply_j 1)))) (set! scalar_multiply_result (conj scalar_multiply_result scalar_multiply_row)) (set! scalar_multiply_i (+ scalar_multiply_i 1)))) (throw (ex-info "return" {:v scalar_multiply_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn multiply [multiply_a multiply_b]
  (binding [multiply_colsA nil multiply_colsB nil multiply_i nil multiply_j nil multiply_k nil multiply_result nil multiply_row nil multiply_rowsA nil multiply_rowsB nil multiply_sum nil] (try (do (set! multiply_rowsA (count multiply_a)) (set! multiply_colsA (count (nth multiply_a 0))) (set! multiply_rowsB (count multiply_b)) (set! multiply_colsB (count (nth multiply_b 0))) (set! multiply_result []) (set! multiply_i 0) (while (< multiply_i multiply_rowsA) (do (set! multiply_row []) (set! multiply_j 0) (while (< multiply_j multiply_colsB) (do (set! multiply_sum 0.0) (set! multiply_k 0) (while (< multiply_k multiply_colsA) (do (set! multiply_sum (+ multiply_sum (* (nth (nth multiply_a multiply_i) multiply_k) (nth (nth multiply_b multiply_k) multiply_j)))) (set! multiply_k (+ multiply_k 1)))) (set! multiply_row (conj multiply_row multiply_sum)) (set! multiply_j (+ multiply_j 1)))) (set! multiply_result (conj multiply_result multiply_row)) (set! multiply_i (+ multiply_i 1)))) (throw (ex-info "return" {:v multiply_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn identity [identity_n]
  (binding [identity_i nil identity_j nil identity_result nil identity_row nil] (try (do (set! identity_result []) (set! identity_i 0) (while (< identity_i identity_n) (do (set! identity_row []) (set! identity_j 0) (while (< identity_j identity_n) (do (if (= identity_i identity_j) (set! identity_row (conj identity_row 1.0)) (set! identity_row (conj identity_row 0.0))) (set! identity_j (+ identity_j 1)))) (set! identity_result (conj identity_result identity_row)) (set! identity_i (+ identity_i 1)))) (throw (ex-info "return" {:v identity_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_matrix]
  (binding [transpose_c nil transpose_cols nil transpose_r nil transpose_result nil transpose_row nil transpose_rows nil] (try (do (set! transpose_rows (count transpose_matrix)) (set! transpose_cols (count (nth transpose_matrix 0))) (set! transpose_result []) (set! transpose_c 0) (while (< transpose_c transpose_cols) (do (set! transpose_row []) (set! transpose_r 0) (while (< transpose_r transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_matrix transpose_r) transpose_c))) (set! transpose_r (+ transpose_r 1)))) (set! transpose_result (conj transpose_result transpose_row)) (set! transpose_c (+ transpose_c 1)))) (throw (ex-info "return" {:v transpose_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn minor [minor_matrix minor_row minor_column]
  (binding [minor_i nil minor_j nil minor_new_row nil minor_result nil] (try (do (set! minor_result []) (set! minor_i 0) (while (< minor_i (count minor_matrix)) (do (when (not= minor_i minor_row) (do (set! minor_new_row []) (set! minor_j 0) (while (< minor_j (count (nth minor_matrix minor_i))) (do (when (not= minor_j minor_column) (set! minor_new_row (conj minor_new_row (nth (nth minor_matrix minor_i) minor_j)))) (set! minor_j (+ minor_j 1)))) (set! minor_result (conj minor_result minor_new_row)))) (set! minor_i (+ minor_i 1)))) (throw (ex-info "return" {:v minor_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn determinant [determinant_matrix]
  (binding [determinant_c nil determinant_det nil determinant_sign nil determinant_sub nil] (try (do (when (= (count determinant_matrix) 1) (throw (ex-info "return" {:v (nth (nth determinant_matrix 0) 0)}))) (set! determinant_det 0.0) (set! determinant_c 0) (while (< determinant_c (count (nth determinant_matrix 0))) (do (set! determinant_sub (minor determinant_matrix 0 determinant_c)) (set! determinant_sign (if (= (mod determinant_c 2) 0) 1.0 (- 1.0))) (set! determinant_det (+ determinant_det (* (* (nth (nth determinant_matrix 0) determinant_c) (determinant determinant_sub)) determinant_sign))) (set! determinant_c (+ determinant_c 1)))) (throw (ex-info "return" {:v determinant_det}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inverse [inverse_matrix]
  (binding [inverse_adjugate nil inverse_cofactors nil inverse_det nil inverse_i nil inverse_j nil inverse_m nil inverse_matrix_minor nil inverse_row nil inverse_sign nil inverse_size nil] (try (do (set! inverse_det (determinant inverse_matrix)) (when (= inverse_det 0.0) (throw (ex-info "return" {:v []}))) (set! inverse_size (count inverse_matrix)) (set! inverse_matrix_minor []) (set! inverse_i 0) (while (< inverse_i inverse_size) (do (set! inverse_row []) (set! inverse_j 0) (while (< inverse_j inverse_size) (do (set! inverse_m (minor inverse_matrix inverse_i inverse_j)) (set! inverse_row (conj inverse_row (determinant inverse_m))) (set! inverse_j (+ inverse_j 1)))) (set! inverse_matrix_minor (conj inverse_matrix_minor inverse_row)) (set! inverse_i (+ inverse_i 1)))) (set! inverse_cofactors []) (set! inverse_i 0) (while (< inverse_i inverse_size) (do (set! inverse_row []) (set! inverse_j 0) (while (< inverse_j inverse_size) (do (set! inverse_sign (if (= (mod (+ inverse_i inverse_j) 2) 0) 1.0 (- 1.0))) (set! inverse_row (conj inverse_row (* (nth (nth inverse_matrix_minor inverse_i) inverse_j) inverse_sign))) (set! inverse_j (+ inverse_j 1)))) (set! inverse_cofactors (conj inverse_cofactors inverse_row)) (set! inverse_i (+ inverse_i 1)))) (set! inverse_adjugate (transpose inverse_cofactors)) (throw (ex-info "return" {:v (scalar_multiply inverse_adjugate (/ 1.0 inverse_det))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_matrix_a nil main_matrix_b nil main_matrix_c nil main_matrix_d nil] (do (set! main_matrix_a [[12.0 10.0] [3.0 9.0]]) (set! main_matrix_b [[3.0 4.0] [7.0 4.0]]) (set! main_matrix_c [[11.0 12.0 13.0 14.0] [21.0 22.0 23.0 24.0] [31.0 32.0 33.0 34.0] [41.0 42.0 43.0 44.0]]) (set! main_matrix_d [[3.0 0.0 2.0] [2.0 0.0 (- 2.0)] [0.0 1.0 1.0]]) (println (str (str "Add Operation, add(matrix_a, matrix_b) = " (str (add [main_matrix_a main_matrix_b]))) " \n")) (println (str (str "Multiply Operation, multiply(matrix_a, matrix_b) = " (str (multiply main_matrix_a main_matrix_b))) " \n")) (println (str (str "Identity: " (str (identity 5))) "\n")) (println (str (str (str (str "Minor of " (str main_matrix_c)) " = ") (str (minor main_matrix_c 1 2))) " \n")) (println (str (str (str (str "Determinant of " (str main_matrix_b)) " = ") (str (determinant main_matrix_b))) " \n")) (println (str (str (str (str "Inverse of " (str main_matrix_d)) " = ") (str (inverse main_matrix_d))) "\n")))))

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
