(ns main (:refer-clojure :exclude [clamp_byte convert_to_negative change_contrast gen_gaussian_kernel img_convolve sort_ints median_filter iabs sobel_filter get_neighbors_pixel pow2 local_binary_value local_binary_pattern]))

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

(declare clamp_byte convert_to_negative change_contrast gen_gaussian_kernel img_convolve sort_ints median_filter iabs sobel_filter get_neighbors_pixel pow2 local_binary_value local_binary_pattern)

(def ^:dynamic change_contrast_h nil)

(def ^:dynamic change_contrast_out nil)

(def ^:dynamic change_contrast_p nil)

(def ^:dynamic change_contrast_row nil)

(def ^:dynamic change_contrast_v nil)

(def ^:dynamic change_contrast_w nil)

(def ^:dynamic change_contrast_x nil)

(def ^:dynamic change_contrast_y nil)

(def ^:dynamic convert_to_negative_h nil)

(def ^:dynamic convert_to_negative_out nil)

(def ^:dynamic convert_to_negative_row nil)

(def ^:dynamic convert_to_negative_w nil)

(def ^:dynamic convert_to_negative_x nil)

(def ^:dynamic convert_to_negative_y nil)

(def ^:dynamic gen_gaussian_kernel_i nil)

(def ^:dynamic gen_gaussian_kernel_j nil)

(def ^:dynamic gen_gaussian_kernel_k nil)

(def ^:dynamic gen_gaussian_kernel_row nil)

(def ^:dynamic get_neighbors_pixel_dx nil)

(def ^:dynamic get_neighbors_pixel_dy nil)

(def ^:dynamic get_neighbors_pixel_h nil)

(def ^:dynamic get_neighbors_pixel_neighbors nil)

(def ^:dynamic get_neighbors_pixel_nx nil)

(def ^:dynamic get_neighbors_pixel_ny nil)

(def ^:dynamic get_neighbors_pixel_val nil)

(def ^:dynamic get_neighbors_pixel_w nil)

(def ^:dynamic img_convolve_acc nil)

(def ^:dynamic img_convolve_h nil)

(def ^:dynamic img_convolve_ix nil)

(def ^:dynamic img_convolve_iy nil)

(def ^:dynamic img_convolve_kx nil)

(def ^:dynamic img_convolve_ky nil)

(def ^:dynamic img_convolve_out nil)

(def ^:dynamic img_convolve_pixel nil)

(def ^:dynamic img_convolve_row nil)

(def ^:dynamic img_convolve_w nil)

(def ^:dynamic img_convolve_x nil)

(def ^:dynamic img_convolve_y nil)

(def ^:dynamic local_binary_pattern_h nil)

(def ^:dynamic local_binary_pattern_out nil)

(def ^:dynamic local_binary_pattern_row nil)

(def ^:dynamic local_binary_pattern_w nil)

(def ^:dynamic local_binary_pattern_x nil)

(def ^:dynamic local_binary_pattern_y nil)

(def ^:dynamic local_binary_value_center nil)

(def ^:dynamic local_binary_value_i nil)

(def ^:dynamic local_binary_value_neighbors nil)

(def ^:dynamic local_binary_value_v nil)

(def ^:dynamic median_filter_h nil)

(def ^:dynamic median_filter_ix nil)

(def ^:dynamic median_filter_iy nil)

(def ^:dynamic median_filter_kx nil)

(def ^:dynamic median_filter_ky nil)

(def ^:dynamic median_filter_offset nil)

(def ^:dynamic median_filter_out nil)

(def ^:dynamic median_filter_pixel nil)

(def ^:dynamic median_filter_row nil)

(def ^:dynamic median_filter_sorted nil)

(def ^:dynamic median_filter_vals nil)

(def ^:dynamic median_filter_w nil)

(def ^:dynamic median_filter_x nil)

(def ^:dynamic median_filter_y nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_r nil)

(def ^:dynamic sobel_filter_gx nil)

(def ^:dynamic sobel_filter_gy nil)

(def ^:dynamic sobel_filter_h nil)

(def ^:dynamic sobel_filter_ix nil)

(def ^:dynamic sobel_filter_iy nil)

(def ^:dynamic sobel_filter_kx nil)

(def ^:dynamic sobel_filter_ky nil)

(def ^:dynamic sobel_filter_out nil)

(def ^:dynamic sobel_filter_pixel nil)

(def ^:dynamic sobel_filter_row nil)

(def ^:dynamic sobel_filter_sx nil)

(def ^:dynamic sobel_filter_sy nil)

(def ^:dynamic sobel_filter_w nil)

(def ^:dynamic sobel_filter_x nil)

(def ^:dynamic sobel_filter_y nil)

(def ^:dynamic sort_ints_arr nil)

(def ^:dynamic sort_ints_i nil)

(def ^:dynamic sort_ints_j nil)

(def ^:dynamic sort_ints_tmp nil)

(defn clamp_byte [clamp_byte_x]
  (try (do (when (< clamp_byte_x 0) (throw (ex-info "return" {:v 0}))) (if (> clamp_byte_x 255) 255 clamp_byte_x)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn convert_to_negative [convert_to_negative_img]
  (binding [convert_to_negative_h nil convert_to_negative_out nil convert_to_negative_row nil convert_to_negative_w nil convert_to_negative_x nil convert_to_negative_y nil] (try (do (set! convert_to_negative_h (count convert_to_negative_img)) (set! convert_to_negative_w (count (nth convert_to_negative_img 0))) (set! convert_to_negative_out []) (set! convert_to_negative_y 0) (while (< convert_to_negative_y convert_to_negative_h) (do (set! convert_to_negative_row []) (set! convert_to_negative_x 0) (while (< convert_to_negative_x convert_to_negative_w) (do (set! convert_to_negative_row (conj convert_to_negative_row (- 255 (nth (nth convert_to_negative_img convert_to_negative_y) convert_to_negative_x)))) (set! convert_to_negative_x (+ convert_to_negative_x 1)))) (set! convert_to_negative_out (conj convert_to_negative_out convert_to_negative_row)) (set! convert_to_negative_y (+ convert_to_negative_y 1)))) (throw (ex-info "return" {:v convert_to_negative_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn change_contrast [change_contrast_img change_contrast_factor]
  (binding [change_contrast_h nil change_contrast_out nil change_contrast_p nil change_contrast_row nil change_contrast_v nil change_contrast_w nil change_contrast_x nil change_contrast_y nil] (try (do (set! change_contrast_h (count change_contrast_img)) (set! change_contrast_w (count (nth change_contrast_img 0))) (set! change_contrast_out []) (set! change_contrast_y 0) (while (< change_contrast_y change_contrast_h) (do (set! change_contrast_row []) (set! change_contrast_x 0) (while (< change_contrast_x change_contrast_w) (do (set! change_contrast_p (nth (nth change_contrast_img change_contrast_y) change_contrast_x)) (set! change_contrast_v (+ (quot (* (- change_contrast_p 128) change_contrast_factor) 100) 128)) (set! change_contrast_v (clamp_byte change_contrast_v)) (set! change_contrast_row (conj change_contrast_row change_contrast_v)) (set! change_contrast_x (+ change_contrast_x 1)))) (set! change_contrast_out (conj change_contrast_out change_contrast_row)) (set! change_contrast_y (+ change_contrast_y 1)))) (throw (ex-info "return" {:v change_contrast_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gen_gaussian_kernel [gen_gaussian_kernel_n gen_gaussian_kernel_sigma]
  (binding [gen_gaussian_kernel_i nil gen_gaussian_kernel_j nil gen_gaussian_kernel_k nil gen_gaussian_kernel_row nil] (try (do (when (= gen_gaussian_kernel_n 3) (throw (ex-info "return" {:v [[(/ 1.0 16.0) (/ 2.0 16.0) (/ 1.0 16.0)] [(/ 2.0 16.0) (/ 4.0 16.0) (/ 2.0 16.0)] [(/ 1.0 16.0) (/ 2.0 16.0) (/ 1.0 16.0)]]}))) (set! gen_gaussian_kernel_k []) (set! gen_gaussian_kernel_i 0) (while (< gen_gaussian_kernel_i gen_gaussian_kernel_n) (do (set! gen_gaussian_kernel_row []) (set! gen_gaussian_kernel_j 0) (while (< gen_gaussian_kernel_j gen_gaussian_kernel_n) (do (set! gen_gaussian_kernel_row (conj gen_gaussian_kernel_row 0.0)) (set! gen_gaussian_kernel_j (+ gen_gaussian_kernel_j 1)))) (set! gen_gaussian_kernel_k (conj gen_gaussian_kernel_k gen_gaussian_kernel_row)) (set! gen_gaussian_kernel_i (+ gen_gaussian_kernel_i 1)))) (throw (ex-info "return" {:v gen_gaussian_kernel_k}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn img_convolve [img_convolve_img img_convolve_kernel]
  (binding [img_convolve_acc nil img_convolve_h nil img_convolve_ix nil img_convolve_iy nil img_convolve_kx nil img_convolve_ky nil img_convolve_out nil img_convolve_pixel nil img_convolve_row nil img_convolve_w nil img_convolve_x nil img_convolve_y nil] (try (do (set! img_convolve_h (count img_convolve_img)) (set! img_convolve_w (count (nth img_convolve_img 0))) (set! img_convolve_out []) (set! img_convolve_y 0) (while (< img_convolve_y img_convolve_h) (do (set! img_convolve_row []) (set! img_convolve_x 0) (while (< img_convolve_x img_convolve_w) (do (set! img_convolve_acc 0.0) (set! img_convolve_ky 0) (while (< img_convolve_ky (count img_convolve_kernel)) (do (set! img_convolve_kx 0) (while (< img_convolve_kx (count (nth img_convolve_kernel 0))) (do (set! img_convolve_iy (- (+ img_convolve_y img_convolve_ky) 1)) (set! img_convolve_ix (- (+ img_convolve_x img_convolve_kx) 1)) (set! img_convolve_pixel 0) (when (and (and (and (>= img_convolve_iy 0) (< img_convolve_iy img_convolve_h)) (>= img_convolve_ix 0)) (< img_convolve_ix img_convolve_w)) (set! img_convolve_pixel (nth (nth img_convolve_img img_convolve_iy) img_convolve_ix))) (set! img_convolve_acc (+ img_convolve_acc (* (nth (nth img_convolve_kernel img_convolve_ky) img_convolve_kx) (* 1.0 img_convolve_pixel)))) (set! img_convolve_kx (+ img_convolve_kx 1)))) (set! img_convolve_ky (+ img_convolve_ky 1)))) (set! img_convolve_row (conj img_convolve_row (Integer/parseInt img_convolve_acc))) (set! img_convolve_x (+ img_convolve_x 1)))) (set! img_convolve_out (conj img_convolve_out img_convolve_row)) (set! img_convolve_y (+ img_convolve_y 1)))) (throw (ex-info "return" {:v img_convolve_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_ints [sort_ints_xs]
  (binding [sort_ints_arr nil sort_ints_i nil sort_ints_j nil sort_ints_tmp nil] (try (do (set! sort_ints_arr sort_ints_xs) (set! sort_ints_i 0) (while (< sort_ints_i (count sort_ints_arr)) (do (set! sort_ints_j 0) (while (< sort_ints_j (- (- (count sort_ints_arr) 1) sort_ints_i)) (do (when (> (nth sort_ints_arr sort_ints_j) (nth sort_ints_arr (+ sort_ints_j 1))) (do (set! sort_ints_tmp (nth sort_ints_arr sort_ints_j)) (set! sort_ints_arr (assoc sort_ints_arr sort_ints_j (nth sort_ints_arr (+ sort_ints_j 1)))) (set! sort_ints_arr (assoc sort_ints_arr (+ sort_ints_j 1) sort_ints_tmp)))) (set! sort_ints_j (+ sort_ints_j 1)))) (set! sort_ints_i (+ sort_ints_i 1)))) (throw (ex-info "return" {:v sort_ints_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn median_filter [median_filter_img median_filter_k]
  (binding [median_filter_h nil median_filter_ix nil median_filter_iy nil median_filter_kx nil median_filter_ky nil median_filter_offset nil median_filter_out nil median_filter_pixel nil median_filter_row nil median_filter_sorted nil median_filter_vals nil median_filter_w nil median_filter_x nil median_filter_y nil] (try (do (set! median_filter_h (count median_filter_img)) (set! median_filter_w (count (nth median_filter_img 0))) (set! median_filter_offset (quot median_filter_k 2)) (set! median_filter_out []) (set! median_filter_y 0) (while (< median_filter_y median_filter_h) (do (set! median_filter_row []) (set! median_filter_x 0) (while (< median_filter_x median_filter_w) (do (set! median_filter_vals []) (set! median_filter_ky 0) (while (< median_filter_ky median_filter_k) (do (set! median_filter_kx 0) (while (< median_filter_kx median_filter_k) (do (set! median_filter_iy (- (+ median_filter_y median_filter_ky) median_filter_offset)) (set! median_filter_ix (- (+ median_filter_x median_filter_kx) median_filter_offset)) (set! median_filter_pixel 0) (when (and (and (and (>= median_filter_iy 0) (< median_filter_iy median_filter_h)) (>= median_filter_ix 0)) (< median_filter_ix median_filter_w)) (set! median_filter_pixel (nth (nth median_filter_img median_filter_iy) median_filter_ix))) (set! median_filter_vals (conj median_filter_vals median_filter_pixel)) (set! median_filter_kx (+ median_filter_kx 1)))) (set! median_filter_ky (+ median_filter_ky 1)))) (set! median_filter_sorted (sort_ints median_filter_vals)) (set! median_filter_row (conj median_filter_row (nth median_filter_sorted (quot (count median_filter_sorted) 2)))) (set! median_filter_x (+ median_filter_x 1)))) (set! median_filter_out (conj median_filter_out median_filter_row)) (set! median_filter_y (+ median_filter_y 1)))) (throw (ex-info "return" {:v median_filter_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn iabs [iabs_x]
  (try (if (< iabs_x 0) (- iabs_x) iabs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sobel_filter [sobel_filter_img]
  (binding [sobel_filter_gx nil sobel_filter_gy nil sobel_filter_h nil sobel_filter_ix nil sobel_filter_iy nil sobel_filter_kx nil sobel_filter_ky nil sobel_filter_out nil sobel_filter_pixel nil sobel_filter_row nil sobel_filter_sx nil sobel_filter_sy nil sobel_filter_w nil sobel_filter_x nil sobel_filter_y nil] (try (do (set! sobel_filter_gx [[1 0 (- 1)] [2 0 (- 2)] [1 0 (- 1)]]) (set! sobel_filter_gy [[1 2 1] [0 0 0] [(- 1) (- 2) (- 1)]]) (set! sobel_filter_h (count sobel_filter_img)) (set! sobel_filter_w (count (nth sobel_filter_img 0))) (set! sobel_filter_out []) (set! sobel_filter_y 0) (while (< sobel_filter_y sobel_filter_h) (do (set! sobel_filter_row []) (set! sobel_filter_x 0) (while (< sobel_filter_x sobel_filter_w) (do (set! sobel_filter_sx 0) (set! sobel_filter_sy 0) (set! sobel_filter_ky 0) (while (< sobel_filter_ky 3) (do (set! sobel_filter_kx 0) (while (< sobel_filter_kx 3) (do (set! sobel_filter_iy (- (+ sobel_filter_y sobel_filter_ky) 1)) (set! sobel_filter_ix (- (+ sobel_filter_x sobel_filter_kx) 1)) (set! sobel_filter_pixel 0) (when (and (and (and (>= sobel_filter_iy 0) (< sobel_filter_iy sobel_filter_h)) (>= sobel_filter_ix 0)) (< sobel_filter_ix sobel_filter_w)) (set! sobel_filter_pixel (nth (nth sobel_filter_img sobel_filter_iy) sobel_filter_ix))) (set! sobel_filter_sx (+ sobel_filter_sx (* (nth (nth sobel_filter_gx sobel_filter_ky) sobel_filter_kx) sobel_filter_pixel))) (set! sobel_filter_sy (+ sobel_filter_sy (* (nth (nth sobel_filter_gy sobel_filter_ky) sobel_filter_kx) sobel_filter_pixel))) (set! sobel_filter_kx (+ sobel_filter_kx 1)))) (set! sobel_filter_ky (+ sobel_filter_ky 1)))) (set! sobel_filter_row (conj sobel_filter_row (+ (iabs sobel_filter_sx) (iabs sobel_filter_sy)))) (set! sobel_filter_x (+ sobel_filter_x 1)))) (set! sobel_filter_out (conj sobel_filter_out sobel_filter_row)) (set! sobel_filter_y (+ sobel_filter_y 1)))) (throw (ex-info "return" {:v sobel_filter_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_neighbors_pixel [get_neighbors_pixel_img get_neighbors_pixel_x get_neighbors_pixel_y]
  (binding [get_neighbors_pixel_dx nil get_neighbors_pixel_dy nil get_neighbors_pixel_h nil get_neighbors_pixel_neighbors nil get_neighbors_pixel_nx nil get_neighbors_pixel_ny nil get_neighbors_pixel_val nil get_neighbors_pixel_w nil] (try (do (set! get_neighbors_pixel_h (count get_neighbors_pixel_img)) (set! get_neighbors_pixel_w (count (nth get_neighbors_pixel_img 0))) (set! get_neighbors_pixel_neighbors []) (set! get_neighbors_pixel_dy (- 1)) (while (<= get_neighbors_pixel_dy 1) (do (set! get_neighbors_pixel_dx (- 1)) (while (<= get_neighbors_pixel_dx 1) (do (when (not (and (= get_neighbors_pixel_dx 0) (= get_neighbors_pixel_dy 0))) (do (set! get_neighbors_pixel_ny (+ get_neighbors_pixel_y get_neighbors_pixel_dy)) (set! get_neighbors_pixel_nx (+ get_neighbors_pixel_x get_neighbors_pixel_dx)) (set! get_neighbors_pixel_val 0) (when (and (and (and (>= get_neighbors_pixel_ny 0) (< get_neighbors_pixel_ny get_neighbors_pixel_h)) (>= get_neighbors_pixel_nx 0)) (< get_neighbors_pixel_nx get_neighbors_pixel_w)) (set! get_neighbors_pixel_val (nth (nth get_neighbors_pixel_img get_neighbors_pixel_ny) get_neighbors_pixel_nx))) (set! get_neighbors_pixel_neighbors (conj get_neighbors_pixel_neighbors get_neighbors_pixel_val)))) (set! get_neighbors_pixel_dx (+ get_neighbors_pixel_dx 1)))) (set! get_neighbors_pixel_dy (+ get_neighbors_pixel_dy 1)))) (throw (ex-info "return" {:v get_neighbors_pixel_neighbors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow2 [pow2_e]
  (binding [pow2_i nil pow2_r nil] (try (do (set! pow2_r 1) (set! pow2_i 0) (while (< pow2_i pow2_e) (do (set! pow2_r (* pow2_r 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn local_binary_value [local_binary_value_img local_binary_value_x local_binary_value_y]
  (binding [local_binary_value_center nil local_binary_value_i nil local_binary_value_neighbors nil local_binary_value_v nil] (try (do (set! local_binary_value_center (nth (nth local_binary_value_img local_binary_value_y) local_binary_value_x)) (set! local_binary_value_neighbors (get_neighbors_pixel local_binary_value_img local_binary_value_x local_binary_value_y)) (set! local_binary_value_v 0) (set! local_binary_value_i 0) (while (< local_binary_value_i (count local_binary_value_neighbors)) (do (when (>= (nth local_binary_value_neighbors local_binary_value_i) local_binary_value_center) (set! local_binary_value_v (+ local_binary_value_v (pow2 local_binary_value_i)))) (set! local_binary_value_i (+ local_binary_value_i 1)))) (throw (ex-info "return" {:v local_binary_value_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn local_binary_pattern [local_binary_pattern_img]
  (binding [local_binary_pattern_h nil local_binary_pattern_out nil local_binary_pattern_row nil local_binary_pattern_w nil local_binary_pattern_x nil local_binary_pattern_y nil] (try (do (set! local_binary_pattern_h (count local_binary_pattern_img)) (set! local_binary_pattern_w (count (nth local_binary_pattern_img 0))) (set! local_binary_pattern_out []) (set! local_binary_pattern_y 0) (while (< local_binary_pattern_y local_binary_pattern_h) (do (set! local_binary_pattern_row []) (set! local_binary_pattern_x 0) (while (< local_binary_pattern_x local_binary_pattern_w) (do (set! local_binary_pattern_row (conj local_binary_pattern_row (local_binary_value local_binary_pattern_img local_binary_pattern_x local_binary_pattern_y))) (set! local_binary_pattern_x (+ local_binary_pattern_x 1)))) (set! local_binary_pattern_out (conj local_binary_pattern_out local_binary_pattern_row)) (set! local_binary_pattern_y (+ local_binary_pattern_y 1)))) (throw (ex-info "return" {:v local_binary_pattern_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_img [[52 55 61] [62 59 55] [63 65 66]])

(def ^:dynamic main_negative (convert_to_negative main_img))

(def ^:dynamic main_contrast (change_contrast main_img 110))

(def ^:dynamic main_kernel (gen_gaussian_kernel 3 1.0))

(def ^:dynamic main_laplace [[0.25 0.5 0.25] [0.5 (- 3.0) 0.5] [0.25 0.5 0.25]])

(def ^:dynamic main_convolved (img_convolve main_img main_laplace))

(def ^:dynamic main_medianed (median_filter main_img 3))

(def ^:dynamic main_sobel (sobel_filter main_img))

(def ^:dynamic main_lbp_img (local_binary_pattern main_img))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_negative)
      (println main_contrast)
      (println main_kernel)
      (println main_convolved)
      (println main_medianed)
      (println main_sobel)
      (println main_lbp_img)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
