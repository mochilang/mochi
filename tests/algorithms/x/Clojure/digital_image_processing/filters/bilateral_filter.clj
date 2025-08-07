(ns main (:refer-clojure :exclude [abs sqrtApprox expApprox vec_gaussian get_slice get_gauss_kernel elementwise_sub elementwise_mul matrix_sum bilateral_filter]))

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

(declare abs sqrtApprox expApprox vec_gaussian get_slice get_gauss_kernel elementwise_sub elementwise_mul matrix_sum bilateral_filter)

(def ^:dynamic bilateral_filter_center nil)

(def ^:dynamic bilateral_filter_gauss_ker nil)

(def ^:dynamic bilateral_filter_img_i nil)

(def ^:dynamic bilateral_filter_img_ig nil)

(def ^:dynamic bilateral_filter_img_s nil)

(def ^:dynamic bilateral_filter_sum_weights nil)

(def ^:dynamic bilateral_filter_val nil)

(def ^:dynamic bilateral_filter_vals nil)

(def ^:dynamic bilateral_filter_weights nil)

(def ^:dynamic elementwise_mul_i nil)

(def ^:dynamic elementwise_mul_j nil)

(def ^:dynamic elementwise_mul_res nil)

(def ^:dynamic elementwise_mul_row nil)

(def ^:dynamic elementwise_sub_i nil)

(def ^:dynamic elementwise_sub_j nil)

(def ^:dynamic elementwise_sub_res nil)

(def ^:dynamic elementwise_sub_row nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic get_gauss_kernel_arr nil)

(def ^:dynamic get_gauss_kernel_di nil)

(def ^:dynamic get_gauss_kernel_dist nil)

(def ^:dynamic get_gauss_kernel_dj nil)

(def ^:dynamic get_gauss_kernel_i nil)

(def ^:dynamic get_gauss_kernel_j nil)

(def ^:dynamic get_gauss_kernel_row nil)

(def ^:dynamic get_slice_half nil)

(def ^:dynamic get_slice_i nil)

(def ^:dynamic get_slice_j nil)

(def ^:dynamic get_slice_row nil)

(def ^:dynamic get_slice_slice nil)

(def ^:dynamic matrix_sum_i nil)

(def ^:dynamic matrix_sum_j nil)

(def ^:dynamic matrix_sum_total nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic vec_gaussian_e nil)

(def ^:dynamic vec_gaussian_i nil)

(def ^:dynamic vec_gaussian_j nil)

(def ^:dynamic vec_gaussian_out nil)

(def ^:dynamic vec_gaussian_row nil)

(def ^:dynamic vec_gaussian_v nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 10) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn expApprox [expApprox_x]
  (binding [expApprox_n nil expApprox_sum nil expApprox_term nil] (try (do (set! expApprox_term 1.0) (set! expApprox_sum 1.0) (set! expApprox_n 1) (while (< expApprox_n 10) (do (set! expApprox_term (quot (* expApprox_term expApprox_x) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (throw (ex-info "return" {:v expApprox_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_gaussian [vec_gaussian_mat vec_gaussian_variance]
  (binding [vec_gaussian_e nil vec_gaussian_i nil vec_gaussian_j nil vec_gaussian_out nil vec_gaussian_row nil vec_gaussian_v nil] (try (do (set! vec_gaussian_i 0) (set! vec_gaussian_out []) (while (< vec_gaussian_i (count vec_gaussian_mat)) (do (set! vec_gaussian_row []) (set! vec_gaussian_j 0) (while (< vec_gaussian_j (count (nth vec_gaussian_mat vec_gaussian_i))) (do (set! vec_gaussian_v (nth (nth vec_gaussian_mat vec_gaussian_i) vec_gaussian_j)) (set! vec_gaussian_e (quot (- (* vec_gaussian_v vec_gaussian_v)) (* 2.0 vec_gaussian_variance))) (set! vec_gaussian_row (vec (concat vec_gaussian_row [(expApprox vec_gaussian_e)]))) (set! vec_gaussian_j (+ vec_gaussian_j 1)))) (set! vec_gaussian_out (vec (concat vec_gaussian_out [vec_gaussian_row]))) (set! vec_gaussian_i (+ vec_gaussian_i 1)))) (throw (ex-info "return" {:v vec_gaussian_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_slice [get_slice_img get_slice_x get_slice_y get_slice_kernel_size]
  (binding [get_slice_half nil get_slice_i nil get_slice_j nil get_slice_row nil get_slice_slice nil] (try (do (set! get_slice_half (quot get_slice_kernel_size 2)) (set! get_slice_i (- get_slice_x get_slice_half)) (set! get_slice_slice []) (while (<= get_slice_i (+ get_slice_x get_slice_half)) (do (set! get_slice_row []) (set! get_slice_j (- get_slice_y get_slice_half)) (while (<= get_slice_j (+ get_slice_y get_slice_half)) (do (set! get_slice_row (vec (concat get_slice_row [(nth (nth get_slice_img get_slice_i) get_slice_j)]))) (set! get_slice_j (+ get_slice_j 1)))) (set! get_slice_slice (vec (concat get_slice_slice [get_slice_row]))) (set! get_slice_i (+ get_slice_i 1)))) (throw (ex-info "return" {:v get_slice_slice}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_gauss_kernel [get_gauss_kernel_kernel_size get_gauss_kernel_spatial_variance]
  (binding [get_gauss_kernel_arr nil get_gauss_kernel_di nil get_gauss_kernel_dist nil get_gauss_kernel_dj nil get_gauss_kernel_i nil get_gauss_kernel_j nil get_gauss_kernel_row nil] (try (do (set! get_gauss_kernel_arr []) (set! get_gauss_kernel_i 0) (while (< get_gauss_kernel_i get_gauss_kernel_kernel_size) (do (set! get_gauss_kernel_row []) (set! get_gauss_kernel_j 0) (while (< get_gauss_kernel_j get_gauss_kernel_kernel_size) (do (set! get_gauss_kernel_di (double (- get_gauss_kernel_i (quot get_gauss_kernel_kernel_size 2)))) (set! get_gauss_kernel_dj (double (- get_gauss_kernel_j (quot get_gauss_kernel_kernel_size 2)))) (set! get_gauss_kernel_dist (sqrtApprox (+ (* get_gauss_kernel_di get_gauss_kernel_di) (* get_gauss_kernel_dj get_gauss_kernel_dj)))) (set! get_gauss_kernel_row (vec (concat get_gauss_kernel_row [get_gauss_kernel_dist]))) (set! get_gauss_kernel_j (+ get_gauss_kernel_j 1)))) (set! get_gauss_kernel_arr (vec (concat get_gauss_kernel_arr [get_gauss_kernel_row]))) (set! get_gauss_kernel_i (+ get_gauss_kernel_i 1)))) (throw (ex-info "return" {:v (vec_gaussian get_gauss_kernel_arr get_gauss_kernel_spatial_variance)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn elementwise_sub [elementwise_sub_mat elementwise_sub_value]
  (binding [elementwise_sub_i nil elementwise_sub_j nil elementwise_sub_res nil elementwise_sub_row nil] (try (do (set! elementwise_sub_res []) (set! elementwise_sub_i 0) (while (< elementwise_sub_i (count elementwise_sub_mat)) (do (set! elementwise_sub_row []) (set! elementwise_sub_j 0) (while (< elementwise_sub_j (count (nth elementwise_sub_mat elementwise_sub_i))) (do (set! elementwise_sub_row (vec (concat elementwise_sub_row [(- (nth (nth elementwise_sub_mat elementwise_sub_i) elementwise_sub_j) elementwise_sub_value)]))) (set! elementwise_sub_j (+ elementwise_sub_j 1)))) (set! elementwise_sub_res (vec (concat elementwise_sub_res [elementwise_sub_row]))) (set! elementwise_sub_i (+ elementwise_sub_i 1)))) (throw (ex-info "return" {:v elementwise_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn elementwise_mul [elementwise_mul_a elementwise_mul_b]
  (binding [elementwise_mul_i nil elementwise_mul_j nil elementwise_mul_res nil elementwise_mul_row nil] (try (do (set! elementwise_mul_res []) (set! elementwise_mul_i 0) (while (< elementwise_mul_i (count elementwise_mul_a)) (do (set! elementwise_mul_row []) (set! elementwise_mul_j 0) (while (< elementwise_mul_j (count (nth elementwise_mul_a elementwise_mul_i))) (do (set! elementwise_mul_row (vec (concat elementwise_mul_row [(* (nth (nth elementwise_mul_a elementwise_mul_i) elementwise_mul_j) (nth (nth elementwise_mul_b elementwise_mul_i) elementwise_mul_j))]))) (set! elementwise_mul_j (+ elementwise_mul_j 1)))) (set! elementwise_mul_res (vec (concat elementwise_mul_res [elementwise_mul_row]))) (set! elementwise_mul_i (+ elementwise_mul_i 1)))) (throw (ex-info "return" {:v elementwise_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_sum [matrix_sum_mat]
  (binding [matrix_sum_i nil matrix_sum_j nil matrix_sum_total nil] (try (do (set! matrix_sum_total 0.0) (set! matrix_sum_i 0) (while (< matrix_sum_i (count matrix_sum_mat)) (do (set! matrix_sum_j 0) (while (< matrix_sum_j (count (nth matrix_sum_mat matrix_sum_i))) (do (set! matrix_sum_total (+ matrix_sum_total (nth (nth matrix_sum_mat matrix_sum_i) matrix_sum_j))) (set! matrix_sum_j (+ matrix_sum_j 1)))) (set! matrix_sum_i (+ matrix_sum_i 1)))) (throw (ex-info "return" {:v matrix_sum_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bilateral_filter [bilateral_filter_img bilateral_filter_spatial_variance bilateral_filter_intensity_variance bilateral_filter_kernel_size]
  (binding [bilateral_filter_center nil bilateral_filter_gauss_ker nil bilateral_filter_img_i nil bilateral_filter_img_ig nil bilateral_filter_img_s nil bilateral_filter_sum_weights nil bilateral_filter_val nil bilateral_filter_vals nil bilateral_filter_weights nil] (try (do (set! bilateral_filter_gauss_ker (get_gauss_kernel bilateral_filter_kernel_size bilateral_filter_spatial_variance)) (set! bilateral_filter_img_s bilateral_filter_img) (set! bilateral_filter_center (nth (nth bilateral_filter_img_s (quot bilateral_filter_kernel_size 2)) (quot bilateral_filter_kernel_size 2))) (set! bilateral_filter_img_i (elementwise_sub bilateral_filter_img_s bilateral_filter_center)) (set! bilateral_filter_img_ig (vec_gaussian bilateral_filter_img_i bilateral_filter_intensity_variance)) (set! bilateral_filter_weights (elementwise_mul bilateral_filter_gauss_ker bilateral_filter_img_ig)) (set! bilateral_filter_vals (elementwise_mul bilateral_filter_img_s bilateral_filter_weights)) (set! bilateral_filter_sum_weights (matrix_sum bilateral_filter_weights)) (set! bilateral_filter_val 0.0) (when (not= bilateral_filter_sum_weights 0.0) (set! bilateral_filter_val (quot (matrix_sum bilateral_filter_vals) bilateral_filter_sum_weights))) (throw (ex-info "return" {:v bilateral_filter_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_img [[0.2 0.3 0.4] [0.3 0.4 0.5] [0.4 0.5 0.6]])

(def ^:dynamic main_result (bilateral_filter main_img 1.0 1.0 3))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_result)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
