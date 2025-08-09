(ns main (:refer-clojure :exclude [sqrt_approx sign vector_norm identity_matrix copy_matrix matmul qr_decomposition print_matrix]))

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

(declare sqrt_approx sign vector_norm identity_matrix copy_matrix matmul qr_decomposition print_matrix)

(def ^:dynamic copy_matrix_i nil)

(def ^:dynamic copy_matrix_j nil)

(def ^:dynamic copy_matrix_mat nil)

(def ^:dynamic copy_matrix_row nil)

(def ^:dynamic identity_matrix_i nil)

(def ^:dynamic identity_matrix_j nil)

(def ^:dynamic identity_matrix_mat nil)

(def ^:dynamic identity_matrix_row nil)

(def ^:dynamic matmul_i nil)

(def ^:dynamic matmul_j nil)

(def ^:dynamic matmul_k nil)

(def ^:dynamic matmul_m nil)

(def ^:dynamic matmul_n nil)

(def ^:dynamic matmul_p nil)

(def ^:dynamic matmul_res nil)

(def ^:dynamic matmul_row nil)

(def ^:dynamic matmul_sum nil)

(def ^:dynamic print_matrix_i nil)

(def ^:dynamic print_matrix_j nil)

(def ^:dynamic print_matrix_line nil)

(def ^:dynamic qr_decomposition_alpha nil)

(def ^:dynamic qr_decomposition_delta nil)

(def ^:dynamic qr_decomposition_e1 nil)

(def ^:dynamic qr_decomposition_i nil)

(def ^:dynamic qr_decomposition_j nil)

(def ^:dynamic qr_decomposition_k nil)

(def ^:dynamic qr_decomposition_m nil)

(def ^:dynamic qr_decomposition_n nil)

(def ^:dynamic qr_decomposition_q nil)

(def ^:dynamic qr_decomposition_qk nil)

(def ^:dynamic qr_decomposition_qk_small nil)

(def ^:dynamic qr_decomposition_r nil)

(def ^:dynamic qr_decomposition_row nil)

(def ^:dynamic qr_decomposition_s nil)

(def ^:dynamic qr_decomposition_size nil)

(def ^:dynamic qr_decomposition_t nil)

(def ^:dynamic qr_decomposition_v nil)

(def ^:dynamic qr_decomposition_vnorm nil)

(def ^:dynamic qr_decomposition_x nil)

(def ^:dynamic sqrt_approx_guess nil)

(def ^:dynamic sqrt_approx_i nil)

(def ^:dynamic vector_norm_i nil)

(def ^:dynamic vector_norm_n nil)

(def ^:dynamic vector_norm_sum nil)

(defn sqrt_approx [sqrt_approx_x]
  (binding [sqrt_approx_guess nil sqrt_approx_i nil] (try (do (when (<= sqrt_approx_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_approx_guess sqrt_approx_x) (set! sqrt_approx_i 0) (while (< sqrt_approx_i 20) (do (set! sqrt_approx_guess (/ (+ sqrt_approx_guess (quot sqrt_approx_x sqrt_approx_guess)) 2.0)) (set! sqrt_approx_i (+ sqrt_approx_i 1)))) (throw (ex-info "return" {:v sqrt_approx_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sign [sign_x]
  (try (if (>= sign_x 0.0) (throw (ex-info "return" {:v 1.0})) (throw (ex-info "return" {:v (- 1.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn vector_norm [vector_norm_v]
  (binding [vector_norm_i nil vector_norm_n nil vector_norm_sum nil] (try (do (set! vector_norm_sum 0.0) (set! vector_norm_i 0) (while (< vector_norm_i (count vector_norm_v)) (do (set! vector_norm_sum (+ vector_norm_sum (* (nth vector_norm_v vector_norm_i) (nth vector_norm_v vector_norm_i)))) (set! vector_norm_i (+ vector_norm_i 1)))) (set! vector_norm_n (sqrt_approx vector_norm_sum)) (throw (ex-info "return" {:v vector_norm_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn identity_matrix [identity_matrix_n]
  (binding [identity_matrix_i nil identity_matrix_j nil identity_matrix_mat nil identity_matrix_row nil] (try (do (set! identity_matrix_mat []) (set! identity_matrix_i 0) (while (< identity_matrix_i identity_matrix_n) (do (set! identity_matrix_row []) (set! identity_matrix_j 0) (while (< identity_matrix_j identity_matrix_n) (do (if (= identity_matrix_i identity_matrix_j) (set! identity_matrix_row (conj identity_matrix_row 1.0)) (set! identity_matrix_row (conj identity_matrix_row 0.0))) (set! identity_matrix_j (+ identity_matrix_j 1)))) (set! identity_matrix_mat (conj identity_matrix_mat identity_matrix_row)) (set! identity_matrix_i (+ identity_matrix_i 1)))) (throw (ex-info "return" {:v identity_matrix_mat}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn copy_matrix [copy_matrix_a]
  (binding [copy_matrix_i nil copy_matrix_j nil copy_matrix_mat nil copy_matrix_row nil] (try (do (set! copy_matrix_mat []) (set! copy_matrix_i 0) (while (< copy_matrix_i (count copy_matrix_a)) (do (set! copy_matrix_row []) (set! copy_matrix_j 0) (while (< copy_matrix_j (count (nth copy_matrix_a copy_matrix_i))) (do (set! copy_matrix_row (conj copy_matrix_row (nth (nth copy_matrix_a copy_matrix_i) copy_matrix_j))) (set! copy_matrix_j (+ copy_matrix_j 1)))) (set! copy_matrix_mat (conj copy_matrix_mat copy_matrix_row)) (set! copy_matrix_i (+ copy_matrix_i 1)))) (throw (ex-info "return" {:v copy_matrix_mat}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matmul [matmul_a matmul_b]
  (binding [matmul_i nil matmul_j nil matmul_k nil matmul_m nil matmul_n nil matmul_p nil matmul_res nil matmul_row nil matmul_sum nil] (try (do (set! matmul_m (count matmul_a)) (set! matmul_n (count (nth matmul_a 0))) (set! matmul_p (count (nth matmul_b 0))) (set! matmul_res []) (set! matmul_i 0) (while (< matmul_i matmul_m) (do (set! matmul_row []) (set! matmul_j 0) (while (< matmul_j matmul_p) (do (set! matmul_sum 0.0) (set! matmul_k 0) (while (< matmul_k matmul_n) (do (set! matmul_sum (+ matmul_sum (* (nth (nth matmul_a matmul_i) matmul_k) (nth (nth matmul_b matmul_k) matmul_j)))) (set! matmul_k (+ matmul_k 1)))) (set! matmul_row (conj matmul_row matmul_sum)) (set! matmul_j (+ matmul_j 1)))) (set! matmul_res (conj matmul_res matmul_row)) (set! matmul_i (+ matmul_i 1)))) (throw (ex-info "return" {:v matmul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn qr_decomposition [qr_decomposition_a]
  (binding [qr_decomposition_alpha nil qr_decomposition_delta nil qr_decomposition_e1 nil qr_decomposition_i nil qr_decomposition_j nil qr_decomposition_k nil qr_decomposition_m nil qr_decomposition_n nil qr_decomposition_q nil qr_decomposition_qk nil qr_decomposition_qk_small nil qr_decomposition_r nil qr_decomposition_row nil qr_decomposition_s nil qr_decomposition_size nil qr_decomposition_t nil qr_decomposition_v nil qr_decomposition_vnorm nil qr_decomposition_x nil] (try (do (set! qr_decomposition_m (count qr_decomposition_a)) (set! qr_decomposition_n (count (nth qr_decomposition_a 0))) (set! qr_decomposition_t (if (< qr_decomposition_m qr_decomposition_n) qr_decomposition_m qr_decomposition_n)) (set! qr_decomposition_q (identity_matrix qr_decomposition_m)) (set! qr_decomposition_r (copy_matrix qr_decomposition_a)) (set! qr_decomposition_k 0) (while (< qr_decomposition_k (- qr_decomposition_t 1)) (do (set! qr_decomposition_x []) (set! qr_decomposition_i qr_decomposition_k) (while (< qr_decomposition_i qr_decomposition_m) (do (set! qr_decomposition_x (conj qr_decomposition_x (nth (nth qr_decomposition_r qr_decomposition_i) qr_decomposition_k))) (set! qr_decomposition_i (+ qr_decomposition_i 1)))) (set! qr_decomposition_e1 []) (set! qr_decomposition_i 0) (while (< qr_decomposition_i (count qr_decomposition_x)) (do (if (= qr_decomposition_i 0) (set! qr_decomposition_e1 (conj qr_decomposition_e1 1.0)) (set! qr_decomposition_e1 (conj qr_decomposition_e1 0.0))) (set! qr_decomposition_i (+ qr_decomposition_i 1)))) (set! qr_decomposition_alpha (vector_norm qr_decomposition_x)) (set! qr_decomposition_s (* (sign (nth qr_decomposition_x 0)) qr_decomposition_alpha)) (set! qr_decomposition_v []) (set! qr_decomposition_i 0) (while (< qr_decomposition_i (count qr_decomposition_x)) (do (set! qr_decomposition_v (conj qr_decomposition_v (+ (nth qr_decomposition_x qr_decomposition_i) (* qr_decomposition_s (nth qr_decomposition_e1 qr_decomposition_i))))) (set! qr_decomposition_i (+ qr_decomposition_i 1)))) (set! qr_decomposition_vnorm (vector_norm qr_decomposition_v)) (set! qr_decomposition_i 0) (while (< qr_decomposition_i (count qr_decomposition_v)) (do (set! qr_decomposition_v (assoc qr_decomposition_v qr_decomposition_i (quot (nth qr_decomposition_v qr_decomposition_i) qr_decomposition_vnorm))) (set! qr_decomposition_i (+ qr_decomposition_i 1)))) (set! qr_decomposition_size (count qr_decomposition_v)) (set! qr_decomposition_qk_small []) (set! qr_decomposition_i 0) (while (< qr_decomposition_i qr_decomposition_size) (do (set! qr_decomposition_row []) (set! qr_decomposition_j 0) (while (< qr_decomposition_j qr_decomposition_size) (do (set! qr_decomposition_delta (if (= qr_decomposition_i qr_decomposition_j) 1.0 0.0)) (set! qr_decomposition_row (conj qr_decomposition_row (- qr_decomposition_delta (* (* 2.0 (nth qr_decomposition_v qr_decomposition_i)) (nth qr_decomposition_v qr_decomposition_j))))) (set! qr_decomposition_j (+ qr_decomposition_j 1)))) (set! qr_decomposition_qk_small (conj qr_decomposition_qk_small qr_decomposition_row)) (set! qr_decomposition_i (+ qr_decomposition_i 1)))) (set! qr_decomposition_qk (identity_matrix qr_decomposition_m)) (set! qr_decomposition_i 0) (while (< qr_decomposition_i qr_decomposition_size) (do (set! qr_decomposition_j 0) (while (< qr_decomposition_j qr_decomposition_size) (do (set! qr_decomposition_qk (assoc-in qr_decomposition_qk [(+ qr_decomposition_k qr_decomposition_i) (+ qr_decomposition_k qr_decomposition_j)] (nth (nth qr_decomposition_qk_small qr_decomposition_i) qr_decomposition_j))) (set! qr_decomposition_j (+ qr_decomposition_j 1)))) (set! qr_decomposition_i (+ qr_decomposition_i 1)))) (set! qr_decomposition_q (matmul qr_decomposition_q qr_decomposition_qk)) (set! qr_decomposition_r (matmul qr_decomposition_qk qr_decomposition_r)) (set! qr_decomposition_k (+ qr_decomposition_k 1)))) (throw (ex-info "return" {:v {:q qr_decomposition_q :r qr_decomposition_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix [print_matrix_mat]
  (binding [print_matrix_i nil print_matrix_j nil print_matrix_line nil] (do (set! print_matrix_i 0) (while (< print_matrix_i (count print_matrix_mat)) (do (set! print_matrix_line "") (set! print_matrix_j 0) (while (< print_matrix_j (count (nth print_matrix_mat print_matrix_i))) (do (set! print_matrix_line (str print_matrix_line (str (nth (nth print_matrix_mat print_matrix_i) print_matrix_j)))) (when (< (+ print_matrix_j 1) (count (nth print_matrix_mat print_matrix_i))) (set! print_matrix_line (str print_matrix_line " "))) (set! print_matrix_j (+ print_matrix_j 1)))) (println print_matrix_line) (set! print_matrix_i (+ print_matrix_i 1)))) print_matrix_mat)))

(def ^:dynamic main_A [[12.0 (- 51.0) 4.0] [6.0 167.0 (- 68.0)] [(- 4.0) 24.0 (- 41.0)]])

(def ^:dynamic main_result (qr_decomposition main_A))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_matrix (:q main_result))
      (print_matrix (:r main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
