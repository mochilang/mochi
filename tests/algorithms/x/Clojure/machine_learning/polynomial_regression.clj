(ns main (:refer-clojure :exclude [design_matrix transpose matmul matvec_mul gaussian_elimination predict]))

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

(declare design_matrix transpose matmul matvec_mul gaussian_elimination predict)

(def ^:dynamic design_matrix_i nil)

(def ^:dynamic design_matrix_j nil)

(def ^:dynamic design_matrix_matrix nil)

(def ^:dynamic design_matrix_pow nil)

(def ^:dynamic design_matrix_row nil)

(def ^:dynamic gaussian_elimination_M nil)

(def ^:dynamic gaussian_elimination_factor nil)

(def ^:dynamic gaussian_elimination_i nil)

(def ^:dynamic gaussian_elimination_i2 nil)

(def ^:dynamic gaussian_elimination_j nil)

(def ^:dynamic gaussian_elimination_j2 nil)

(def ^:dynamic gaussian_elimination_k nil)

(def ^:dynamic gaussian_elimination_l nil)

(def ^:dynamic gaussian_elimination_n nil)

(def ^:dynamic gaussian_elimination_rowj nil)

(def ^:dynamic gaussian_elimination_rowk nil)

(def ^:dynamic gaussian_elimination_sum nil)

(def ^:dynamic gaussian_elimination_t nil)

(def ^:dynamic gaussian_elimination_x nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_ys nil)

(def ^:dynamic matmul_i nil)

(def ^:dynamic matmul_j nil)

(def ^:dynamic matmul_k nil)

(def ^:dynamic matmul_m nil)

(def ^:dynamic matmul_n nil)

(def ^:dynamic matmul_p nil)

(def ^:dynamic matmul_result nil)

(def ^:dynamic matmul_row nil)

(def ^:dynamic matmul_sum nil)

(def ^:dynamic matvec_mul_i nil)

(def ^:dynamic matvec_mul_j nil)

(def ^:dynamic matvec_mul_m nil)

(def ^:dynamic matvec_mul_n nil)

(def ^:dynamic matvec_mul_result nil)

(def ^:dynamic matvec_mul_sum nil)

(def ^:dynamic predict_i nil)

(def ^:dynamic predict_j nil)

(def ^:dynamic predict_pow nil)

(def ^:dynamic predict_result nil)

(def ^:dynamic predict_sum nil)

(def ^:dynamic predict_x nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_result nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(defn design_matrix [design_matrix_xs design_matrix_degree]
  (binding [design_matrix_i nil design_matrix_j nil design_matrix_matrix nil design_matrix_pow nil design_matrix_row nil] (try (do (set! design_matrix_i 0) (set! design_matrix_matrix []) (while (< design_matrix_i (count design_matrix_xs)) (do (set! design_matrix_row []) (set! design_matrix_j 0) (set! design_matrix_pow 1.0) (while (<= design_matrix_j design_matrix_degree) (do (set! design_matrix_row (conj design_matrix_row design_matrix_pow)) (set! design_matrix_pow (* design_matrix_pow (nth design_matrix_xs design_matrix_i))) (set! design_matrix_j (+ design_matrix_j 1)))) (set! design_matrix_matrix (conj design_matrix_matrix design_matrix_row)) (set! design_matrix_i (+ design_matrix_i 1)))) (throw (ex-info "return" {:v design_matrix_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_matrix]
  (binding [transpose_cols nil transpose_i nil transpose_j nil transpose_result nil transpose_row nil transpose_rows nil] (try (do (set! transpose_rows (count transpose_matrix)) (set! transpose_cols (count (nth transpose_matrix 0))) (set! transpose_j 0) (set! transpose_result []) (while (< transpose_j transpose_cols) (do (set! transpose_row []) (set! transpose_i 0) (while (< transpose_i transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_matrix transpose_i) transpose_j))) (set! transpose_i (+ transpose_i 1)))) (set! transpose_result (conj transpose_result transpose_row)) (set! transpose_j (+ transpose_j 1)))) (throw (ex-info "return" {:v transpose_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matmul [matmul_A matmul_B]
  (binding [matmul_i nil matmul_j nil matmul_k nil matmul_m nil matmul_n nil matmul_p nil matmul_result nil matmul_row nil matmul_sum nil] (try (do (set! matmul_n (count matmul_A)) (set! matmul_m (count (nth matmul_A 0))) (set! matmul_p (count (nth matmul_B 0))) (set! matmul_i 0) (set! matmul_result []) (while (< matmul_i matmul_n) (do (set! matmul_row []) (set! matmul_k 0) (while (< matmul_k matmul_p) (do (set! matmul_sum 0.0) (set! matmul_j 0) (while (< matmul_j matmul_m) (do (set! matmul_sum (+ matmul_sum (* (nth (nth matmul_A matmul_i) matmul_j) (nth (nth matmul_B matmul_j) matmul_k)))) (set! matmul_j (+ matmul_j 1)))) (set! matmul_row (conj matmul_row matmul_sum)) (set! matmul_k (+ matmul_k 1)))) (set! matmul_result (conj matmul_result matmul_row)) (set! matmul_i (+ matmul_i 1)))) (throw (ex-info "return" {:v matmul_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matvec_mul [matvec_mul_A matvec_mul_v]
  (binding [matvec_mul_i nil matvec_mul_j nil matvec_mul_m nil matvec_mul_n nil matvec_mul_result nil matvec_mul_sum nil] (try (do (set! matvec_mul_n (count matvec_mul_A)) (set! matvec_mul_m (count (nth matvec_mul_A 0))) (set! matvec_mul_i 0) (set! matvec_mul_result []) (while (< matvec_mul_i matvec_mul_n) (do (set! matvec_mul_sum 0.0) (set! matvec_mul_j 0) (while (< matvec_mul_j matvec_mul_m) (do (set! matvec_mul_sum (+ matvec_mul_sum (* (nth (nth matvec_mul_A matvec_mul_i) matvec_mul_j) (nth matvec_mul_v matvec_mul_j)))) (set! matvec_mul_j (+ matvec_mul_j 1)))) (set! matvec_mul_result (conj matvec_mul_result matvec_mul_sum)) (set! matvec_mul_i (+ matvec_mul_i 1)))) (throw (ex-info "return" {:v matvec_mul_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gaussian_elimination [gaussian_elimination_A gaussian_elimination_b]
  (binding [gaussian_elimination_M nil gaussian_elimination_factor nil gaussian_elimination_i nil gaussian_elimination_i2 nil gaussian_elimination_j nil gaussian_elimination_j2 nil gaussian_elimination_k nil gaussian_elimination_l nil gaussian_elimination_n nil gaussian_elimination_rowj nil gaussian_elimination_rowk nil gaussian_elimination_sum nil gaussian_elimination_t nil gaussian_elimination_x nil] (try (do (set! gaussian_elimination_n (count gaussian_elimination_A)) (set! gaussian_elimination_M []) (set! gaussian_elimination_i 0) (while (< gaussian_elimination_i gaussian_elimination_n) (do (set! gaussian_elimination_M (conj gaussian_elimination_M (conj (nth gaussian_elimination_A gaussian_elimination_i) (nth gaussian_elimination_b gaussian_elimination_i)))) (set! gaussian_elimination_i (+ gaussian_elimination_i 1)))) (set! gaussian_elimination_k 0) (while (< gaussian_elimination_k gaussian_elimination_n) (do (set! gaussian_elimination_j (+ gaussian_elimination_k 1)) (while (< gaussian_elimination_j gaussian_elimination_n) (do (set! gaussian_elimination_factor (quot (nth (nth gaussian_elimination_M gaussian_elimination_j) gaussian_elimination_k) (nth (nth gaussian_elimination_M gaussian_elimination_k) gaussian_elimination_k))) (set! gaussian_elimination_rowj (nth gaussian_elimination_M gaussian_elimination_j)) (set! gaussian_elimination_rowk (nth gaussian_elimination_M gaussian_elimination_k)) (set! gaussian_elimination_l gaussian_elimination_k) (while (<= gaussian_elimination_l gaussian_elimination_n) (do (set! gaussian_elimination_rowj (assoc gaussian_elimination_rowj gaussian_elimination_l (- (nth gaussian_elimination_rowj gaussian_elimination_l) (* gaussian_elimination_factor (nth gaussian_elimination_rowk gaussian_elimination_l))))) (set! gaussian_elimination_l (+ gaussian_elimination_l 1)))) (set! gaussian_elimination_M (assoc gaussian_elimination_M gaussian_elimination_j gaussian_elimination_rowj)) (set! gaussian_elimination_j (+ gaussian_elimination_j 1)))) (set! gaussian_elimination_k (+ gaussian_elimination_k 1)))) (set! gaussian_elimination_x []) (set! gaussian_elimination_t 0) (while (< gaussian_elimination_t gaussian_elimination_n) (do (set! gaussian_elimination_x (conj gaussian_elimination_x 0.0)) (set! gaussian_elimination_t (+ gaussian_elimination_t 1)))) (set! gaussian_elimination_i2 (- gaussian_elimination_n 1)) (while (>= gaussian_elimination_i2 0) (do (set! gaussian_elimination_sum (nth (nth gaussian_elimination_M gaussian_elimination_i2) gaussian_elimination_n)) (set! gaussian_elimination_j2 (+ gaussian_elimination_i2 1)) (while (< gaussian_elimination_j2 gaussian_elimination_n) (do (set! gaussian_elimination_sum (- gaussian_elimination_sum (* (nth (nth gaussian_elimination_M gaussian_elimination_i2) gaussian_elimination_j2) (nth gaussian_elimination_x gaussian_elimination_j2)))) (set! gaussian_elimination_j2 (+ gaussian_elimination_j2 1)))) (set! gaussian_elimination_x (assoc gaussian_elimination_x gaussian_elimination_i2 (quot gaussian_elimination_sum (nth (nth gaussian_elimination_M gaussian_elimination_i2) gaussian_elimination_i2)))) (set! gaussian_elimination_i2 (- gaussian_elimination_i2 1)))) (throw (ex-info "return" {:v gaussian_elimination_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict [predict_xs predict_coeffs]
  (binding [predict_i nil predict_j nil predict_pow nil predict_result nil predict_sum nil predict_x nil] (try (do (set! predict_i 0) (set! predict_result []) (while (< predict_i (count predict_xs)) (do (set! predict_x (nth predict_xs predict_i)) (set! predict_j 0) (set! predict_pow 1.0) (set! predict_sum 0.0) (while (< predict_j (count predict_coeffs)) (do (set! predict_sum (+ predict_sum (* (nth predict_coeffs predict_j) predict_pow))) (set! predict_pow (* predict_pow predict_x)) (set! predict_j (+ predict_j 1)))) (set! predict_result (conj predict_result predict_sum)) (set! predict_i (+ predict_i 1)))) (throw (ex-info "return" {:v predict_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_xs [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0])

(def ^:dynamic main_ys [])

(def ^:dynamic main_i 0)

(def ^:dynamic main_X (design_matrix main_xs 3))

(def ^:dynamic main_Xt (transpose main_X))

(def ^:dynamic main_XtX (matmul main_Xt main_X))

(def ^:dynamic main_Xty (matvec_mul main_Xt main_ys))

(def ^:dynamic main_coeffs (gaussian_elimination main_XtX main_Xty))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i (count main_xs)) (do (def ^:dynamic main_x (nth main_xs main_i)) (def main_ys (conj main_ys (- (+ (- (* (* main_x main_x) main_x) (* (* 2.0 main_x) main_x)) (* 3.0 main_x)) 5.0))) (def main_i (+ main_i 1))))
      (println (str main_coeffs))
      (println (str (predict [(- 1.0)] main_coeffs)))
      (println (str (predict [(- 2.0)] main_coeffs)))
      (println (str (predict [6.0] main_coeffs)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
