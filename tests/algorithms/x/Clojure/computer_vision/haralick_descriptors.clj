(ns main (:refer-clojure :exclude [abs_int sqrt ln matrix_concurrency haralick_descriptors]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare abs_int sqrt ln matrix_concurrency haralick_descriptors)

(def ^:dynamic haralick_descriptors_adiff nil)

(def ^:dynamic haralick_descriptors_cols nil)

(def ^:dynamic haralick_descriptors_contrast nil)

(def ^:dynamic haralick_descriptors_correlation nil)

(def ^:dynamic haralick_descriptors_diff nil)

(def ^:dynamic haralick_descriptors_dissimilarity nil)

(def ^:dynamic haralick_descriptors_energy nil)

(def ^:dynamic haralick_descriptors_entropy nil)

(def ^:dynamic haralick_descriptors_homogeneity nil)

(def ^:dynamic haralick_descriptors_i nil)

(def ^:dynamic haralick_descriptors_inverse_difference nil)

(def ^:dynamic haralick_descriptors_j nil)

(def ^:dynamic haralick_descriptors_maximum_prob nil)

(def ^:dynamic haralick_descriptors_rows nil)

(def ^:dynamic haralick_descriptors_val nil)

(def ^:dynamic ln_e nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_n nil)

(def ^:dynamic ln_result nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic main_idx nil)

(def ^:dynamic matrix_concurrency_base nil)

(def ^:dynamic matrix_concurrency_matrix nil)

(def ^:dynamic matrix_concurrency_max_val nil)

(def ^:dynamic matrix_concurrency_offset nil)

(def ^:dynamic matrix_concurrency_offset_x nil)

(def ^:dynamic matrix_concurrency_offset_y nil)

(def ^:dynamic matrix_concurrency_row nil)

(def ^:dynamic matrix_concurrency_size nil)

(def ^:dynamic matrix_concurrency_total nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(defn abs_int [abs_int_n]
  (try (if (< abs_int_n 0) (- abs_int_n) abs_int_n) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (/ sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_e nil ln_k nil ln_n nil ln_result nil ln_term nil ln_y nil] (try (do (when (<= ln_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! ln_e 2.718281828) (set! ln_n 0) (set! ln_y ln_x) (while (>= ln_y ln_e) (do (set! ln_y (/ ln_y ln_e)) (set! ln_n (+ ln_n 1)))) (while (<= ln_y (/ 1.0 ln_e)) (do (set! ln_y (* ln_y ln_e)) (set! ln_n (- ln_n 1)))) (set! ln_y (- ln_y 1.0)) (set! ln_term ln_y) (set! ln_result 0.0) (set! ln_k 1) (while (<= ln_k 20) (do (if (= (mod ln_k 2) 1) (set! ln_result (+ ln_result (/ ln_term (* 1.0 ln_k)))) (set! ln_result (- ln_result (/ ln_term (* 1.0 ln_k))))) (set! ln_term (* ln_term ln_y)) (set! ln_k (+ ln_k 1)))) (throw (ex-info "return" {:v (+ ln_result (* 1.0 ln_n))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_concurrency [matrix_concurrency_image matrix_concurrency_coord]
  (binding [matrix_concurrency_base nil matrix_concurrency_matrix nil matrix_concurrency_max_val nil matrix_concurrency_offset nil matrix_concurrency_offset_x nil matrix_concurrency_offset_y nil matrix_concurrency_row nil matrix_concurrency_size nil matrix_concurrency_total nil] (try (do (set! matrix_concurrency_offset_x (nth matrix_concurrency_coord 0)) (set! matrix_concurrency_offset_y (nth matrix_concurrency_coord 1)) (set! matrix_concurrency_max_val 0) (dotimes [r (count matrix_concurrency_image)] (dotimes [c (count (nth matrix_concurrency_image r))] (when (> (nth (nth matrix_concurrency_image r) c) matrix_concurrency_max_val) (set! matrix_concurrency_max_val (nth (nth matrix_concurrency_image r) c))))) (set! matrix_concurrency_size (+ matrix_concurrency_max_val 1)) (set! matrix_concurrency_matrix []) (dotimes [i matrix_concurrency_size] (do (set! matrix_concurrency_row []) (dotimes [j matrix_concurrency_size] (set! matrix_concurrency_row (conj matrix_concurrency_row 0.0))) (set! matrix_concurrency_matrix (conj matrix_concurrency_matrix matrix_concurrency_row)))) (doseq [x (range 1 (- (count matrix_concurrency_image) 1))] (doseq [y (range 1 (- (count (nth matrix_concurrency_image x)) 1))] (do (set! matrix_concurrency_base (nth (nth matrix_concurrency_image x) y)) (set! matrix_concurrency_offset (nth (nth matrix_concurrency_image (+ x matrix_concurrency_offset_x)) (+ y matrix_concurrency_offset_y))) (set! matrix_concurrency_matrix (assoc-in matrix_concurrency_matrix [matrix_concurrency_base matrix_concurrency_offset] (+ (nth (nth matrix_concurrency_matrix matrix_concurrency_base) matrix_concurrency_offset) 1.0)))))) (set! matrix_concurrency_total 0.0) (dotimes [i matrix_concurrency_size] (dotimes [j matrix_concurrency_size] (set! matrix_concurrency_total (+ matrix_concurrency_total (nth (nth matrix_concurrency_matrix i) j))))) (when (= matrix_concurrency_total 0.0) (throw (ex-info "return" {:v matrix_concurrency_matrix}))) (dotimes [i matrix_concurrency_size] (dotimes [j matrix_concurrency_size] (set! matrix_concurrency_matrix (assoc-in matrix_concurrency_matrix [i j] (/ (nth (nth matrix_concurrency_matrix i) j) matrix_concurrency_total))))) (throw (ex-info "return" {:v matrix_concurrency_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn haralick_descriptors [haralick_descriptors_matrix]
  (binding [haralick_descriptors_adiff nil haralick_descriptors_cols nil haralick_descriptors_contrast nil haralick_descriptors_correlation nil haralick_descriptors_diff nil haralick_descriptors_dissimilarity nil haralick_descriptors_energy nil haralick_descriptors_entropy nil haralick_descriptors_homogeneity nil haralick_descriptors_i nil haralick_descriptors_inverse_difference nil haralick_descriptors_j nil haralick_descriptors_maximum_prob nil haralick_descriptors_rows nil haralick_descriptors_val nil] (try (do (set! haralick_descriptors_rows (count haralick_descriptors_matrix)) (set! haralick_descriptors_cols (count (nth haralick_descriptors_matrix 0))) (set! haralick_descriptors_maximum_prob 0.0) (set! haralick_descriptors_correlation 0.0) (set! haralick_descriptors_energy 0.0) (set! haralick_descriptors_contrast 0.0) (set! haralick_descriptors_dissimilarity 0.0) (set! haralick_descriptors_inverse_difference 0.0) (set! haralick_descriptors_homogeneity 0.0) (set! haralick_descriptors_entropy 0.0) (set! haralick_descriptors_i 0) (while (< haralick_descriptors_i haralick_descriptors_rows) (do (set! haralick_descriptors_j 0) (while (< haralick_descriptors_j haralick_descriptors_cols) (do (set! haralick_descriptors_val (nth (nth haralick_descriptors_matrix haralick_descriptors_i) haralick_descriptors_j)) (when (> haralick_descriptors_val haralick_descriptors_maximum_prob) (set! haralick_descriptors_maximum_prob haralick_descriptors_val)) (set! haralick_descriptors_correlation (+ haralick_descriptors_correlation (* (* (* 1.0 haralick_descriptors_i) haralick_descriptors_j) haralick_descriptors_val))) (set! haralick_descriptors_energy (+ haralick_descriptors_energy (* haralick_descriptors_val haralick_descriptors_val))) (set! haralick_descriptors_diff (- haralick_descriptors_i haralick_descriptors_j)) (set! haralick_descriptors_adiff (abs_int haralick_descriptors_diff)) (set! haralick_descriptors_contrast (+ haralick_descriptors_contrast (* haralick_descriptors_val (* (* 1.0 haralick_descriptors_diff) haralick_descriptors_diff)))) (set! haralick_descriptors_dissimilarity (+ haralick_descriptors_dissimilarity (* haralick_descriptors_val (* 1.0 haralick_descriptors_adiff)))) (set! haralick_descriptors_inverse_difference (+ haralick_descriptors_inverse_difference (/ haralick_descriptors_val (+ 1.0 (* 1.0 haralick_descriptors_adiff))))) (set! haralick_descriptors_homogeneity (+ haralick_descriptors_homogeneity (/ haralick_descriptors_val (+ 1.0 (* (* 1.0 haralick_descriptors_diff) haralick_descriptors_diff))))) (when (> haralick_descriptors_val 0.0) (set! haralick_descriptors_entropy (- haralick_descriptors_entropy (* haralick_descriptors_val (ln haralick_descriptors_val))))) (set! haralick_descriptors_j (+ haralick_descriptors_j 1)))) (set! haralick_descriptors_i (+ haralick_descriptors_i 1)))) (throw (ex-info "return" {:v [haralick_descriptors_maximum_prob haralick_descriptors_correlation haralick_descriptors_energy haralick_descriptors_contrast haralick_descriptors_dissimilarity haralick_descriptors_inverse_difference haralick_descriptors_homogeneity haralick_descriptors_entropy]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_image [[0 1 0] [1 0 1] [0 1 0]])

(def ^:dynamic main_glcm (matrix_concurrency main_image [0 1]))

(def ^:dynamic main_descriptors (haralick_descriptors main_glcm))

(def ^:dynamic main_idx 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_idx (count main_descriptors)) (do (println (str (nth main_descriptors main_idx))) (def main_idx (+ main_idx 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
