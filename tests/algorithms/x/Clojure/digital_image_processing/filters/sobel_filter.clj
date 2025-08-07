(ns main (:refer-clojure :exclude [absf sqrtApprox atanApprox atan2Approx zeros pad_edge img_convolve abs_matrix max_matrix scale_matrix sobel_filter print_matrix_int print_matrix_float main]))

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

(declare absf sqrtApprox atanApprox atan2Approx zeros pad_edge img_convolve abs_matrix max_matrix scale_matrix sobel_filter print_matrix_int print_matrix_float main)

(def ^:dynamic abs_matrix_h nil)

(def ^:dynamic abs_matrix_out nil)

(def ^:dynamic abs_matrix_v nil)

(def ^:dynamic abs_matrix_w nil)

(def ^:dynamic abs_matrix_x nil)

(def ^:dynamic abs_matrix_y nil)

(def ^:dynamic atan2Approx_a nil)

(def ^:dynamic img_convolve_h nil)

(def ^:dynamic img_convolve_i nil)

(def ^:dynamic img_convolve_j nil)

(def ^:dynamic img_convolve_k nil)

(def ^:dynamic img_convolve_out nil)

(def ^:dynamic img_convolve_pad nil)

(def ^:dynamic img_convolve_padded nil)

(def ^:dynamic img_convolve_sum nil)

(def ^:dynamic img_convolve_w nil)

(def ^:dynamic img_convolve_x nil)

(def ^:dynamic img_convolve_y nil)

(def ^:dynamic main_img nil)

(def ^:dynamic main_mag nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_theta nil)

(def ^:dynamic max_matrix_max_val nil)

(def ^:dynamic max_matrix_x nil)

(def ^:dynamic max_matrix_y nil)

(def ^:dynamic pad_edge_h nil)

(def ^:dynamic pad_edge_out nil)

(def ^:dynamic pad_edge_sx nil)

(def ^:dynamic pad_edge_sy nil)

(def ^:dynamic pad_edge_w nil)

(def ^:dynamic pad_edge_x nil)

(def ^:dynamic pad_edge_y nil)

(def ^:dynamic print_matrix_float_line nil)

(def ^:dynamic print_matrix_float_x nil)

(def ^:dynamic print_matrix_float_y nil)

(def ^:dynamic print_matrix_int_line nil)

(def ^:dynamic print_matrix_int_x nil)

(def ^:dynamic print_matrix_int_y nil)

(def ^:dynamic scale_matrix_h nil)

(def ^:dynamic scale_matrix_out nil)

(def ^:dynamic scale_matrix_w nil)

(def ^:dynamic scale_matrix_x nil)

(def ^:dynamic scale_matrix_y nil)

(def ^:dynamic sobel_filter_dst_x nil)

(def ^:dynamic sobel_filter_dst_y nil)

(def ^:dynamic sobel_filter_gx nil)

(def ^:dynamic sobel_filter_gy nil)

(def ^:dynamic sobel_filter_h nil)

(def ^:dynamic sobel_filter_img nil)

(def ^:dynamic sobel_filter_kernel_x nil)

(def ^:dynamic sobel_filter_kernel_y nil)

(def ^:dynamic sobel_filter_mag nil)

(def ^:dynamic sobel_filter_max_m nil)

(def ^:dynamic sobel_filter_max_x nil)

(def ^:dynamic sobel_filter_max_y nil)

(def ^:dynamic sobel_filter_row nil)

(def ^:dynamic sobel_filter_theta nil)

(def ^:dynamic sobel_filter_w nil)

(def ^:dynamic sobel_filter_x nil)

(def ^:dynamic sobel_filter_x0 nil)

(def ^:dynamic sobel_filter_y nil)

(def ^:dynamic sobel_filter_y0 nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic zeros_m nil)

(def ^:dynamic zeros_row nil)

(def ^:dynamic zeros_x nil)

(def ^:dynamic zeros_y nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn atanApprox [atanApprox_x]
  (try (do (when (> atanApprox_x 1.0) (throw (ex-info "return" {:v (- (/ main_PI 2.0) (quot atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28)))}))) (if (< atanApprox_x (- 1.0)) (- (/ (- main_PI) 2.0) (quot atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28))) (quot atanApprox_x (+ 1.0 (* (* 0.28 atanApprox_x) atanApprox_x))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn atan2Approx [atan2Approx_y atan2Approx_x]
  (binding [atan2Approx_a nil] (try (do (when (= atan2Approx_x 0.0) (do (when (> atan2Approx_y 0.0) (throw (ex-info "return" {:v (/ main_PI 2.0)}))) (when (< atan2Approx_y 0.0) (throw (ex-info "return" {:v (/ (- main_PI) 2.0)}))) (throw (ex-info "return" {:v 0.0})))) (set! atan2Approx_a (atanApprox (quot atan2Approx_y atan2Approx_x))) (when (> atan2Approx_x 0.0) (throw (ex-info "return" {:v atan2Approx_a}))) (if (>= atan2Approx_y 0.0) (+ atan2Approx_a main_PI) (- atan2Approx_a main_PI))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zeros [zeros_h zeros_w]
  (binding [zeros_m nil zeros_row nil zeros_x nil zeros_y nil] (try (do (set! zeros_m []) (set! zeros_y 0) (while (< zeros_y zeros_h) (do (set! zeros_row []) (set! zeros_x 0) (while (< zeros_x zeros_w) (do (set! zeros_row (conj zeros_row 0.0)) (set! zeros_x (+ zeros_x 1)))) (set! zeros_m (conj zeros_m zeros_row)) (set! zeros_y (+ zeros_y 1)))) (throw (ex-info "return" {:v zeros_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pad_edge [pad_edge_img pad_edge_pad]
  (binding [pad_edge_h nil pad_edge_out nil pad_edge_sx nil pad_edge_sy nil pad_edge_w nil pad_edge_x nil pad_edge_y nil] (try (do (set! pad_edge_h (count pad_edge_img)) (set! pad_edge_w (count (nth pad_edge_img 0))) (set! pad_edge_out (zeros (+ pad_edge_h (* pad_edge_pad 2)) (+ pad_edge_w (* pad_edge_pad 2)))) (set! pad_edge_y 0) (while (< pad_edge_y (+ pad_edge_h (* pad_edge_pad 2))) (do (set! pad_edge_x 0) (while (< pad_edge_x (+ pad_edge_w (* pad_edge_pad 2))) (do (set! pad_edge_sy (- pad_edge_y pad_edge_pad)) (when (< pad_edge_sy 0) (set! pad_edge_sy 0)) (when (>= pad_edge_sy pad_edge_h) (set! pad_edge_sy (- pad_edge_h 1))) (set! pad_edge_sx (- pad_edge_x pad_edge_pad)) (when (< pad_edge_sx 0) (set! pad_edge_sx 0)) (when (>= pad_edge_sx pad_edge_w) (set! pad_edge_sx (- pad_edge_w 1))) (set! pad_edge_out (assoc-in pad_edge_out [pad_edge_y pad_edge_x] (nth (nth pad_edge_img pad_edge_sy) pad_edge_sx))) (set! pad_edge_x (+ pad_edge_x 1)))) (set! pad_edge_y (+ pad_edge_y 1)))) (throw (ex-info "return" {:v pad_edge_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn img_convolve [img_convolve_img img_convolve_kernel]
  (binding [img_convolve_h nil img_convolve_i nil img_convolve_j nil img_convolve_k nil img_convolve_out nil img_convolve_pad nil img_convolve_padded nil img_convolve_sum nil img_convolve_w nil img_convolve_x nil img_convolve_y nil] (try (do (set! img_convolve_h (count img_convolve_img)) (set! img_convolve_w (count (nth img_convolve_img 0))) (set! img_convolve_k (count img_convolve_kernel)) (set! img_convolve_pad (quot img_convolve_k 2)) (set! img_convolve_padded (pad_edge img_convolve_img img_convolve_pad)) (set! img_convolve_out (zeros img_convolve_h img_convolve_w)) (set! img_convolve_y 0) (while (< img_convolve_y img_convolve_h) (do (set! img_convolve_x 0) (while (< img_convolve_x img_convolve_w) (do (set! img_convolve_sum 0.0) (set! img_convolve_i 0) (while (< img_convolve_i img_convolve_k) (do (set! img_convolve_j 0) (while (< img_convolve_j img_convolve_k) (do (set! img_convolve_sum (+ img_convolve_sum (* (nth (nth img_convolve_padded (+ img_convolve_y img_convolve_i)) (+ img_convolve_x img_convolve_j)) (double (nth (nth img_convolve_kernel img_convolve_i) img_convolve_j))))) (set! img_convolve_j (+ img_convolve_j 1)))) (set! img_convolve_i (+ img_convolve_i 1)))) (set! img_convolve_out (assoc-in img_convolve_out [img_convolve_y img_convolve_x] img_convolve_sum)) (set! img_convolve_x (+ img_convolve_x 1)))) (set! img_convolve_y (+ img_convolve_y 1)))) (throw (ex-info "return" {:v img_convolve_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_matrix [abs_matrix_mat]
  (binding [abs_matrix_h nil abs_matrix_out nil abs_matrix_v nil abs_matrix_w nil abs_matrix_x nil abs_matrix_y nil] (try (do (set! abs_matrix_h (count abs_matrix_mat)) (set! abs_matrix_w (count (nth abs_matrix_mat 0))) (set! abs_matrix_out (zeros abs_matrix_h abs_matrix_w)) (set! abs_matrix_y 0) (while (< abs_matrix_y abs_matrix_h) (do (set! abs_matrix_x 0) (while (< abs_matrix_x abs_matrix_w) (do (set! abs_matrix_v (nth (nth abs_matrix_mat abs_matrix_y) abs_matrix_x)) (if (< abs_matrix_v 0.0) (set! abs_matrix_out (assoc-in abs_matrix_out [abs_matrix_y abs_matrix_x] (- abs_matrix_v))) (set! abs_matrix_out (assoc-in abs_matrix_out [abs_matrix_y abs_matrix_x] abs_matrix_v))) (set! abs_matrix_x (+ abs_matrix_x 1)))) (set! abs_matrix_y (+ abs_matrix_y 1)))) (throw (ex-info "return" {:v abs_matrix_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_matrix [max_matrix_mat]
  (binding [max_matrix_max_val nil max_matrix_x nil max_matrix_y nil] (try (do (set! max_matrix_max_val (nth (nth max_matrix_mat 0) 0)) (set! max_matrix_y 0) (while (< max_matrix_y (count max_matrix_mat)) (do (set! max_matrix_x 0) (while (< max_matrix_x (count (nth max_matrix_mat 0))) (do (when (> (nth (nth max_matrix_mat max_matrix_y) max_matrix_x) max_matrix_max_val) (set! max_matrix_max_val (nth (nth max_matrix_mat max_matrix_y) max_matrix_x))) (set! max_matrix_x (+ max_matrix_x 1)))) (set! max_matrix_y (+ max_matrix_y 1)))) (throw (ex-info "return" {:v max_matrix_max_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn scale_matrix [scale_matrix_mat scale_matrix_factor]
  (binding [scale_matrix_h nil scale_matrix_out nil scale_matrix_w nil scale_matrix_x nil scale_matrix_y nil] (try (do (set! scale_matrix_h (count scale_matrix_mat)) (set! scale_matrix_w (count (nth scale_matrix_mat 0))) (set! scale_matrix_out (zeros scale_matrix_h scale_matrix_w)) (set! scale_matrix_y 0) (while (< scale_matrix_y scale_matrix_h) (do (set! scale_matrix_x 0) (while (< scale_matrix_x scale_matrix_w) (do (set! scale_matrix_out (assoc-in scale_matrix_out [scale_matrix_y scale_matrix_x] (* (nth (nth scale_matrix_mat scale_matrix_y) scale_matrix_x) scale_matrix_factor))) (set! scale_matrix_x (+ scale_matrix_x 1)))) (set! scale_matrix_y (+ scale_matrix_y 1)))) (throw (ex-info "return" {:v scale_matrix_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sobel_filter [sobel_filter_image]
  (binding [sobel_filter_dst_x nil sobel_filter_dst_y nil sobel_filter_gx nil sobel_filter_gy nil sobel_filter_h nil sobel_filter_img nil sobel_filter_kernel_x nil sobel_filter_kernel_y nil sobel_filter_mag nil sobel_filter_max_m nil sobel_filter_max_x nil sobel_filter_max_y nil sobel_filter_row nil sobel_filter_theta nil sobel_filter_w nil sobel_filter_x nil sobel_filter_x0 nil sobel_filter_y nil sobel_filter_y0 nil] (try (do (set! sobel_filter_h (count sobel_filter_image)) (set! sobel_filter_w (count (nth sobel_filter_image 0))) (set! sobel_filter_img []) (set! sobel_filter_y0 0) (while (< sobel_filter_y0 sobel_filter_h) (do (set! sobel_filter_row []) (set! sobel_filter_x0 0) (while (< sobel_filter_x0 sobel_filter_w) (do (set! sobel_filter_row (conj sobel_filter_row (double (nth (nth sobel_filter_image sobel_filter_y0) sobel_filter_x0)))) (set! sobel_filter_x0 (+ sobel_filter_x0 1)))) (set! sobel_filter_img (conj sobel_filter_img sobel_filter_row)) (set! sobel_filter_y0 (+ sobel_filter_y0 1)))) (set! sobel_filter_kernel_x [[(- 1) 0 1] [(- 2) 0 2] [(- 1) 0 1]]) (set! sobel_filter_kernel_y [[1 2 1] [0 0 0] [(- 1) (- 2) (- 1)]]) (set! sobel_filter_dst_x (abs_matrix (img_convolve sobel_filter_img sobel_filter_kernel_x))) (set! sobel_filter_dst_y (abs_matrix (img_convolve sobel_filter_img sobel_filter_kernel_y))) (set! sobel_filter_max_x (max_matrix sobel_filter_dst_x)) (set! sobel_filter_max_y (max_matrix sobel_filter_dst_y)) (set! sobel_filter_dst_x (scale_matrix sobel_filter_dst_x (/ 255.0 sobel_filter_max_x))) (set! sobel_filter_dst_y (scale_matrix sobel_filter_dst_y (/ 255.0 sobel_filter_max_y))) (set! sobel_filter_mag (zeros sobel_filter_h sobel_filter_w)) (set! sobel_filter_theta (zeros sobel_filter_h sobel_filter_w)) (set! sobel_filter_y 0) (while (< sobel_filter_y sobel_filter_h) (do (set! sobel_filter_x 0) (while (< sobel_filter_x sobel_filter_w) (do (set! sobel_filter_gx (nth (nth sobel_filter_dst_x sobel_filter_y) sobel_filter_x)) (set! sobel_filter_gy (nth (nth sobel_filter_dst_y sobel_filter_y) sobel_filter_x)) (set! sobel_filter_mag (assoc-in sobel_filter_mag [sobel_filter_y sobel_filter_x] (sqrtApprox (+ (* sobel_filter_gx sobel_filter_gx) (* sobel_filter_gy sobel_filter_gy))))) (set! sobel_filter_theta (assoc-in sobel_filter_theta [sobel_filter_y sobel_filter_x] (atan2Approx sobel_filter_gy sobel_filter_gx))) (set! sobel_filter_x (+ sobel_filter_x 1)))) (set! sobel_filter_y (+ sobel_filter_y 1)))) (set! sobel_filter_max_m (max_matrix sobel_filter_mag)) (set! sobel_filter_mag (scale_matrix sobel_filter_mag (/ 255.0 sobel_filter_max_m))) (throw (ex-info "return" {:v [sobel_filter_mag sobel_filter_theta]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix_int [print_matrix_int_mat]
  (binding [print_matrix_int_line nil print_matrix_int_x nil print_matrix_int_y nil] (do (set! print_matrix_int_y 0) (while (< print_matrix_int_y (count print_matrix_int_mat)) (do (set! print_matrix_int_line "") (set! print_matrix_int_x 0) (while (< print_matrix_int_x (count (nth print_matrix_int_mat print_matrix_int_y))) (do (set! print_matrix_int_line (str print_matrix_int_line (str (long (nth (nth print_matrix_int_mat print_matrix_int_y) print_matrix_int_x))))) (when (< print_matrix_int_x (- (count (nth print_matrix_int_mat print_matrix_int_y)) 1)) (set! print_matrix_int_line (str print_matrix_int_line " "))) (set! print_matrix_int_x (+ print_matrix_int_x 1)))) (println print_matrix_int_line) (set! print_matrix_int_y (+ print_matrix_int_y 1)))))))

(defn print_matrix_float [print_matrix_float_mat]
  (binding [print_matrix_float_line nil print_matrix_float_x nil print_matrix_float_y nil] (do (set! print_matrix_float_y 0) (while (< print_matrix_float_y (count print_matrix_float_mat)) (do (set! print_matrix_float_line "") (set! print_matrix_float_x 0) (while (< print_matrix_float_x (count (nth print_matrix_float_mat print_matrix_float_y))) (do (set! print_matrix_float_line (str print_matrix_float_line (str (nth (nth print_matrix_float_mat print_matrix_float_y) print_matrix_float_x)))) (when (< print_matrix_float_x (- (count (nth print_matrix_float_mat print_matrix_float_y)) 1)) (set! print_matrix_float_line (str print_matrix_float_line " "))) (set! print_matrix_float_x (+ print_matrix_float_x 1)))) (println print_matrix_float_line) (set! print_matrix_float_y (+ print_matrix_float_y 1)))))))

(defn main []
  (binding [main_img nil main_mag nil main_res nil main_theta nil] (do (set! main_img [[10 10 10 10 10] [10 50 50 50 10] [10 50 80 50 10] [10 50 50 50 10] [10 10 10 10 10]]) (set! main_res (sobel_filter main_img)) (set! main_mag (nth main_res 0)) (set! main_theta (nth main_res 1)) (print_matrix_int main_mag) (print_matrix_float main_theta))))

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
