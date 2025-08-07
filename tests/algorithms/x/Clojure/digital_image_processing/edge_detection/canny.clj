(ns main (:refer-clojure :exclude [sqrtApprox atanApprox atan2Approx deg zero_matrix convolve gaussian_blur sobel_filter suppress_non_maximum double_threshold track_edge canny print_image]))

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

(declare sqrtApprox atanApprox atan2Approx deg zero_matrix convolve gaussian_blur sobel_filter suppress_non_maximum double_threshold track_edge canny print_image)

(def ^:dynamic atan2Approx_r nil)

(def ^:dynamic canny_blurred nil)

(def ^:dynamic canny_direction nil)

(def ^:dynamic canny_grad nil)

(def ^:dynamic canny_h nil)

(def ^:dynamic canny_sob nil)

(def ^:dynamic canny_suppressed nil)

(def ^:dynamic canny_w nil)

(def ^:dynamic convolve_h nil)

(def ^:dynamic convolve_k nil)

(def ^:dynamic convolve_kx nil)

(def ^:dynamic convolve_ky nil)

(def ^:dynamic convolve_out nil)

(def ^:dynamic convolve_pad nil)

(def ^:dynamic convolve_pixel nil)

(def ^:dynamic convolve_sum nil)

(def ^:dynamic convolve_w nil)

(def ^:dynamic convolve_weight nil)

(def ^:dynamic convolve_x nil)

(def ^:dynamic convolve_y nil)

(def ^:dynamic double_threshold_c nil)

(def ^:dynamic double_threshold_img nil)

(def ^:dynamic double_threshold_r nil)

(def ^:dynamic double_threshold_v nil)

(def ^:dynamic print_image_c nil)

(def ^:dynamic print_image_line nil)

(def ^:dynamic print_image_r nil)

(def ^:dynamic sobel_filter_dir nil)

(def ^:dynamic sobel_filter_grad nil)

(def ^:dynamic sobel_filter_gx nil)

(def ^:dynamic sobel_filter_gxx nil)

(def ^:dynamic sobel_filter_gy nil)

(def ^:dynamic sobel_filter_gyy nil)

(def ^:dynamic sobel_filter_h nil)

(def ^:dynamic sobel_filter_i nil)

(def ^:dynamic sobel_filter_j nil)

(def ^:dynamic sobel_filter_w nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic suppress_non_maximum_angle nil)

(def ^:dynamic suppress_non_maximum_c nil)

(def ^:dynamic suppress_non_maximum_dest nil)

(def ^:dynamic suppress_non_maximum_p nil)

(def ^:dynamic suppress_non_maximum_q nil)

(def ^:dynamic suppress_non_maximum_r nil)

(def ^:dynamic track_edge_c nil)

(def ^:dynamic track_edge_img nil)

(def ^:dynamic track_edge_r nil)

(def ^:dynamic zero_matrix_i nil)

(def ^:dynamic zero_matrix_j nil)

(def ^:dynamic zero_matrix_out nil)

(def ^:dynamic zero_matrix_row nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn atanApprox [atanApprox_x]
  (try (do (when (> atanApprox_x 1.0) (throw (ex-info "return" {:v (- (/ main_PI 2.0) (quot atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28)))}))) (if (< atanApprox_x (- 1.0)) (- (/ (- main_PI) 2.0) (quot atanApprox_x (+ (* atanApprox_x atanApprox_x) 0.28))) (quot atanApprox_x (+ 1.0 (* (* 0.28 atanApprox_x) atanApprox_x))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn atan2Approx [atan2Approx_y atan2Approx_x]
  (binding [atan2Approx_r nil] (try (do (when (> atan2Approx_x 0.0) (do (set! atan2Approx_r (atanApprox (quot atan2Approx_y atan2Approx_x))) (throw (ex-info "return" {:v atan2Approx_r})))) (when (< atan2Approx_x 0.0) (do (when (>= atan2Approx_y 0.0) (throw (ex-info "return" {:v (+ (atanApprox (quot atan2Approx_y atan2Approx_x)) main_PI)}))) (throw (ex-info "return" {:v (- (atanApprox (quot atan2Approx_y atan2Approx_x)) main_PI)})))) (when (> atan2Approx_y 0.0) (throw (ex-info "return" {:v (/ main_PI 2.0)}))) (if (< atan2Approx_y 0.0) (/ (- main_PI) 2.0) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn deg [deg_rad]
  (try (throw (ex-info "return" {:v (/ (* deg_rad 180.0) main_PI)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_GAUSSIAN_KERNEL [[0.0625 0.125 0.0625] [0.125 0.25 0.125] [0.0625 0.125 0.0625]])

(def ^:dynamic main_SOBEL_GX [[(- 1.0) 0.0 1.0] [(- 2.0) 0.0 2.0] [(- 1.0) 0.0 1.0]])

(def ^:dynamic main_SOBEL_GY [[1.0 2.0 1.0] [0.0 0.0 0.0] [(- 1.0) (- 2.0) (- 1.0)]])

(defn zero_matrix [zero_matrix_h zero_matrix_w]
  (binding [zero_matrix_i nil zero_matrix_j nil zero_matrix_out nil zero_matrix_row nil] (try (do (set! zero_matrix_out []) (set! zero_matrix_i 0) (while (< zero_matrix_i zero_matrix_h) (do (set! zero_matrix_row []) (set! zero_matrix_j 0) (while (< zero_matrix_j zero_matrix_w) (do (set! zero_matrix_row (conj zero_matrix_row 0.0)) (set! zero_matrix_j (+ zero_matrix_j 1)))) (set! zero_matrix_out (conj zero_matrix_out zero_matrix_row)) (set! zero_matrix_i (+ zero_matrix_i 1)))) (throw (ex-info "return" {:v zero_matrix_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convolve [convolve_img convolve_kernel]
  (binding [convolve_h nil convolve_k nil convolve_kx nil convolve_ky nil convolve_out nil convolve_pad nil convolve_pixel nil convolve_sum nil convolve_w nil convolve_weight nil convolve_x nil convolve_y nil] (try (do (set! convolve_h (count convolve_img)) (set! convolve_w (count (nth convolve_img 0))) (set! convolve_k (count convolve_kernel)) (set! convolve_pad (quot convolve_k 2)) (set! convolve_out (zero_matrix convolve_h convolve_w)) (set! convolve_y convolve_pad) (while (< convolve_y (- convolve_h convolve_pad)) (do (set! convolve_x convolve_pad) (while (< convolve_x (- convolve_w convolve_pad)) (do (set! convolve_sum 0.0) (set! convolve_ky 0) (while (< convolve_ky convolve_k) (do (set! convolve_kx 0) (while (< convolve_kx convolve_k) (do (set! convolve_pixel (nth (nth convolve_img (+ (- convolve_y convolve_pad) convolve_ky)) (+ (- convolve_x convolve_pad) convolve_kx))) (set! convolve_weight (nth (nth convolve_kernel convolve_ky) convolve_kx)) (set! convolve_sum (+ convolve_sum (* convolve_pixel convolve_weight))) (set! convolve_kx (+ convolve_kx 1)))) (set! convolve_ky (+ convolve_ky 1)))) (set! convolve_out (assoc-in convolve_out [convolve_y convolve_x] convolve_sum)) (set! convolve_x (+ convolve_x 1)))) (set! convolve_y (+ convolve_y 1)))) (throw (ex-info "return" {:v convolve_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gaussian_blur [gaussian_blur_img]
  (try (throw (ex-info "return" {:v (convolve gaussian_blur_img main_GAUSSIAN_KERNEL)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sobel_filter [sobel_filter_img]
  (binding [sobel_filter_dir nil sobel_filter_grad nil sobel_filter_gx nil sobel_filter_gxx nil sobel_filter_gy nil sobel_filter_gyy nil sobel_filter_h nil sobel_filter_i nil sobel_filter_j nil sobel_filter_w nil] (try (do (set! sobel_filter_gx (convolve sobel_filter_img main_SOBEL_GX)) (set! sobel_filter_gy (convolve sobel_filter_img main_SOBEL_GY)) (set! sobel_filter_h (count sobel_filter_img)) (set! sobel_filter_w (count (nth sobel_filter_img 0))) (set! sobel_filter_grad (zero_matrix sobel_filter_h sobel_filter_w)) (set! sobel_filter_dir (zero_matrix sobel_filter_h sobel_filter_w)) (set! sobel_filter_i 0) (while (< sobel_filter_i sobel_filter_h) (do (set! sobel_filter_j 0) (while (< sobel_filter_j sobel_filter_w) (do (set! sobel_filter_gxx (nth (nth sobel_filter_gx sobel_filter_i) sobel_filter_j)) (set! sobel_filter_gyy (nth (nth sobel_filter_gy sobel_filter_i) sobel_filter_j)) (set! sobel_filter_grad (assoc-in sobel_filter_grad [sobel_filter_i sobel_filter_j] (sqrtApprox (+ (* sobel_filter_gxx sobel_filter_gxx) (* sobel_filter_gyy sobel_filter_gyy))))) (set! sobel_filter_dir (assoc-in sobel_filter_dir [sobel_filter_i sobel_filter_j] (+ (deg (atan2Approx sobel_filter_gyy sobel_filter_gxx)) 180.0))) (set! sobel_filter_j (+ sobel_filter_j 1)))) (set! sobel_filter_i (+ sobel_filter_i 1)))) (throw (ex-info "return" {:v {"dir" sobel_filter_dir "grad" sobel_filter_grad}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn suppress_non_maximum [suppress_non_maximum_h suppress_non_maximum_w suppress_non_maximum_direction suppress_non_maximum_grad]
  (binding [suppress_non_maximum_angle nil suppress_non_maximum_c nil suppress_non_maximum_dest nil suppress_non_maximum_p nil suppress_non_maximum_q nil suppress_non_maximum_r nil] (try (do (set! suppress_non_maximum_dest (zero_matrix suppress_non_maximum_h suppress_non_maximum_w)) (set! suppress_non_maximum_r 1) (while (< suppress_non_maximum_r (- suppress_non_maximum_h 1)) (do (set! suppress_non_maximum_c 1) (while (< suppress_non_maximum_c (- suppress_non_maximum_w 1)) (do (set! suppress_non_maximum_angle (nth (nth suppress_non_maximum_direction suppress_non_maximum_r) suppress_non_maximum_c)) (set! suppress_non_maximum_q 0.0) (set! suppress_non_maximum_p 0.0) (if (or (or (and (>= suppress_non_maximum_angle 0.0) (< suppress_non_maximum_angle 22.5)) (and (>= suppress_non_maximum_angle 157.5) (<= suppress_non_maximum_angle 180.0))) (>= suppress_non_maximum_angle 337.5)) (do (set! suppress_non_maximum_q (nth (nth suppress_non_maximum_grad suppress_non_maximum_r) (+ suppress_non_maximum_c 1))) (set! suppress_non_maximum_p (nth (nth suppress_non_maximum_grad suppress_non_maximum_r) (- suppress_non_maximum_c 1)))) (if (or (and (>= suppress_non_maximum_angle 22.5) (< suppress_non_maximum_angle 67.5)) (and (>= suppress_non_maximum_angle 202.5) (< suppress_non_maximum_angle 247.5))) (do (set! suppress_non_maximum_q (nth (nth suppress_non_maximum_grad (+ suppress_non_maximum_r 1)) (- suppress_non_maximum_c 1))) (set! suppress_non_maximum_p (nth (nth suppress_non_maximum_grad (- suppress_non_maximum_r 1)) (+ suppress_non_maximum_c 1)))) (if (or (and (>= suppress_non_maximum_angle 67.5) (< suppress_non_maximum_angle 112.5)) (and (>= suppress_non_maximum_angle 247.5) (< suppress_non_maximum_angle 292.5))) (do (set! suppress_non_maximum_q (nth (nth suppress_non_maximum_grad (+ suppress_non_maximum_r 1)) suppress_non_maximum_c)) (set! suppress_non_maximum_p (nth (nth suppress_non_maximum_grad (- suppress_non_maximum_r 1)) suppress_non_maximum_c))) (do (set! suppress_non_maximum_q (nth (nth suppress_non_maximum_grad (- suppress_non_maximum_r 1)) (- suppress_non_maximum_c 1))) (set! suppress_non_maximum_p (nth (nth suppress_non_maximum_grad (+ suppress_non_maximum_r 1)) (+ suppress_non_maximum_c 1))))))) (when (and (>= (nth (nth suppress_non_maximum_grad suppress_non_maximum_r) suppress_non_maximum_c) suppress_non_maximum_q) (>= (nth (nth suppress_non_maximum_grad suppress_non_maximum_r) suppress_non_maximum_c) suppress_non_maximum_p)) (set! suppress_non_maximum_dest (assoc-in suppress_non_maximum_dest [suppress_non_maximum_r suppress_non_maximum_c] (nth (nth suppress_non_maximum_grad suppress_non_maximum_r) suppress_non_maximum_c)))) (set! suppress_non_maximum_c (+ suppress_non_maximum_c 1)))) (set! suppress_non_maximum_r (+ suppress_non_maximum_r 1)))) (throw (ex-info "return" {:v suppress_non_maximum_dest}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn double_threshold [double_threshold_h double_threshold_w double_threshold_img_p double_threshold_low double_threshold_high double_threshold_weak double_threshold_strong]
  (binding [double_threshold_c nil double_threshold_img nil double_threshold_r nil double_threshold_v nil] (do (set! double_threshold_img double_threshold_img_p) (set! double_threshold_r 0) (while (< double_threshold_r double_threshold_h) (do (set! double_threshold_c 0) (while (< double_threshold_c double_threshold_w) (do (set! double_threshold_v (nth (nth double_threshold_img double_threshold_r) double_threshold_c)) (if (>= double_threshold_v double_threshold_high) (set! double_threshold_img (assoc-in double_threshold_img [double_threshold_r double_threshold_c] double_threshold_strong)) (if (< double_threshold_v double_threshold_low) (set! double_threshold_img (assoc-in double_threshold_img [double_threshold_r double_threshold_c] 0.0)) (set! double_threshold_img (assoc-in double_threshold_img [double_threshold_r double_threshold_c] double_threshold_weak)))) (set! double_threshold_c (+ double_threshold_c 1)))) (set! double_threshold_r (+ double_threshold_r 1)))))))

(defn track_edge [track_edge_h track_edge_w track_edge_img_p track_edge_weak track_edge_strong]
  (binding [track_edge_c nil track_edge_img nil track_edge_r nil] (do (set! track_edge_img track_edge_img_p) (set! track_edge_r 1) (while (< track_edge_r (- track_edge_h 1)) (do (set! track_edge_c 1) (while (< track_edge_c (- track_edge_w 1)) (do (when (= (nth (nth track_edge_img track_edge_r) track_edge_c) track_edge_weak) (if (or (or (or (or (or (or (or (= (nth (nth track_edge_img (+ track_edge_r 1)) track_edge_c) track_edge_strong) (= (nth (nth track_edge_img (- track_edge_r 1)) track_edge_c) track_edge_strong)) (= (nth (nth track_edge_img track_edge_r) (+ track_edge_c 1)) track_edge_strong)) (= (nth (nth track_edge_img track_edge_r) (- track_edge_c 1)) track_edge_strong)) (= (nth (nth track_edge_img (- track_edge_r 1)) (- track_edge_c 1)) track_edge_strong)) (= (nth (nth track_edge_img (- track_edge_r 1)) (+ track_edge_c 1)) track_edge_strong)) (= (nth (nth track_edge_img (+ track_edge_r 1)) (- track_edge_c 1)) track_edge_strong)) (= (nth (nth track_edge_img (+ track_edge_r 1)) (+ track_edge_c 1)) track_edge_strong)) (set! track_edge_img (assoc-in track_edge_img [track_edge_r track_edge_c] track_edge_strong)) (set! track_edge_img (assoc-in track_edge_img [track_edge_r track_edge_c] 0.0)))) (set! track_edge_c (+ track_edge_c 1)))) (set! track_edge_r (+ track_edge_r 1)))))))

(defn canny [canny_image canny_low canny_high canny_weak canny_strong]
  (binding [canny_blurred nil canny_direction nil canny_grad nil canny_h nil canny_sob nil canny_suppressed nil canny_w nil] (try (do (set! canny_blurred (gaussian_blur canny_image)) (set! canny_sob (sobel_filter canny_blurred)) (set! canny_grad (get canny_sob "grad")) (set! canny_direction (get canny_sob "dir")) (set! canny_h (count canny_image)) (set! canny_w (count (nth canny_image 0))) (set! canny_suppressed (suppress_non_maximum canny_h canny_w canny_direction canny_grad)) (double_threshold canny_h canny_w canny_suppressed canny_low canny_high canny_weak canny_strong) (track_edge canny_h canny_w canny_suppressed canny_weak canny_strong) (throw (ex-info "return" {:v canny_suppressed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_image [print_image_img]
  (binding [print_image_c nil print_image_line nil print_image_r nil] (do (set! print_image_r 0) (while (< print_image_r (count print_image_img)) (do (set! print_image_c 0) (set! print_image_line "") (while (< print_image_c (count (nth print_image_img print_image_r))) (do (set! print_image_line (str (str print_image_line (str (long (nth (nth print_image_img print_image_r) print_image_c)))) " ")) (set! print_image_c (+ print_image_c 1)))) (println print_image_line) (set! print_image_r (+ print_image_r 1)))))))

(def ^:dynamic main_image [[0.0 0.0 0.0 0.0 0.0] [0.0 255.0 255.0 255.0 0.0] [0.0 255.0 255.0 255.0 0.0] [0.0 255.0 255.0 255.0 0.0] [0.0 0.0 0.0 0.0 0.0]])

(def ^:dynamic main_edges (canny main_image 20.0 40.0 128.0 255.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_image main_edges)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
