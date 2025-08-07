(ns main (:refer-clojure :exclude [round_int zeros warp convolve horn_schunck print_matrix main]))

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

(declare round_int zeros warp convolve horn_schunck print_matrix main)

(def ^:dynamic convolve_h nil)

(def ^:dynamic convolve_ix nil)

(def ^:dynamic convolve_iy nil)

(def ^:dynamic convolve_kh nil)

(def ^:dynamic convolve_kw nil)

(def ^:dynamic convolve_kx nil)

(def ^:dynamic convolve_ky nil)

(def ^:dynamic convolve_out nil)

(def ^:dynamic convolve_px nil)

(def ^:dynamic convolve_py nil)

(def ^:dynamic convolve_row nil)

(def ^:dynamic convolve_s nil)

(def ^:dynamic convolve_w nil)

(def ^:dynamic convolve_x nil)

(def ^:dynamic convolve_y nil)

(def ^:dynamic horn_schunck_au nil)

(def ^:dynamic horn_schunck_av nil)

(def ^:dynamic horn_schunck_avg_u nil)

(def ^:dynamic horn_schunck_avg_v nil)

(def ^:dynamic horn_schunck_denom nil)

(def ^:dynamic horn_schunck_dt nil)

(def ^:dynamic horn_schunck_dt1 nil)

(def ^:dynamic horn_schunck_dt2 nil)

(def ^:dynamic horn_schunck_dx nil)

(def ^:dynamic horn_schunck_dx1 nil)

(def ^:dynamic horn_schunck_dx2 nil)

(def ^:dynamic horn_schunck_dy nil)

(def ^:dynamic horn_schunck_dy1 nil)

(def ^:dynamic horn_schunck_dy2 nil)

(def ^:dynamic horn_schunck_h nil)

(def ^:dynamic horn_schunck_it nil)

(def ^:dynamic horn_schunck_kernel_t nil)

(def ^:dynamic horn_schunck_kernel_x nil)

(def ^:dynamic horn_schunck_kernel_y nil)

(def ^:dynamic horn_schunck_laplacian nil)

(def ^:dynamic horn_schunck_numer nil)

(def ^:dynamic horn_schunck_u nil)

(def ^:dynamic horn_schunck_upd nil)

(def ^:dynamic horn_schunck_v nil)

(def ^:dynamic horn_schunck_w nil)

(def ^:dynamic horn_schunck_warped nil)

(def ^:dynamic horn_schunck_x nil)

(def ^:dynamic horn_schunck_y nil)

(def ^:dynamic main_flows nil)

(def ^:dynamic main_image0 nil)

(def ^:dynamic main_image1 nil)

(def ^:dynamic main_u nil)

(def ^:dynamic main_v nil)

(def ^:dynamic print_matrix_line nil)

(def ^:dynamic print_matrix_row nil)

(def ^:dynamic print_matrix_x nil)

(def ^:dynamic print_matrix_y nil)

(def ^:dynamic warp_h nil)

(def ^:dynamic warp_out nil)

(def ^:dynamic warp_row nil)

(def ^:dynamic warp_sx nil)

(def ^:dynamic warp_sy nil)

(def ^:dynamic warp_w nil)

(def ^:dynamic warp_x nil)

(def ^:dynamic warp_y nil)

(def ^:dynamic zeros_i nil)

(def ^:dynamic zeros_j nil)

(def ^:dynamic zeros_res nil)

(def ^:dynamic zeros_row nil)

(defn round_int [round_int_x]
  (try (if (>= round_int_x 0.0) (Integer/parseInt (+ round_int_x 0.5)) (Integer/parseInt (- round_int_x 0.5))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn zeros [zeros_rows zeros_cols]
  (binding [zeros_i nil zeros_j nil zeros_res nil zeros_row nil] (try (do (set! zeros_res []) (set! zeros_i 0) (while (< zeros_i zeros_rows) (do (set! zeros_row []) (set! zeros_j 0) (while (< zeros_j zeros_cols) (do (set! zeros_row (conj zeros_row 0.0)) (set! zeros_j (+ zeros_j 1)))) (set! zeros_res (conj zeros_res zeros_row)) (set! zeros_i (+ zeros_i 1)))) (throw (ex-info "return" {:v zeros_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn warp [warp_image warp_h_flow warp_v_flow]
  (binding [warp_h nil warp_out nil warp_row nil warp_sx nil warp_sy nil warp_w nil warp_x nil warp_y nil] (try (do (set! warp_h (count warp_image)) (set! warp_w (count (nth warp_image 0))) (set! warp_out []) (set! warp_y 0) (while (< warp_y warp_h) (do (set! warp_row []) (set! warp_x 0) (while (< warp_x warp_w) (do (set! warp_sx (- warp_x (round_int (nth (nth warp_h_flow warp_y) warp_x)))) (set! warp_sy (- warp_y (round_int (nth (nth warp_v_flow warp_y) warp_x)))) (if (and (and (and (>= warp_sx 0) (< warp_sx warp_w)) (>= warp_sy 0)) (< warp_sy warp_h)) (set! warp_row (conj warp_row (nth (nth warp_image warp_sy) warp_sx))) (set! warp_row (conj warp_row 0.0))) (set! warp_x (+ warp_x 1)))) (set! warp_out (conj warp_out warp_row)) (set! warp_y (+ warp_y 1)))) (throw (ex-info "return" {:v warp_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convolve [convolve_img convolve_ker]
  (binding [convolve_h nil convolve_ix nil convolve_iy nil convolve_kh nil convolve_kw nil convolve_kx nil convolve_ky nil convolve_out nil convolve_px nil convolve_py nil convolve_row nil convolve_s nil convolve_w nil convolve_x nil convolve_y nil] (try (do (set! convolve_h (count convolve_img)) (set! convolve_w (count (nth convolve_img 0))) (set! convolve_kh (count convolve_ker)) (set! convolve_kw (count (nth convolve_ker 0))) (set! convolve_py (quot convolve_kh 2)) (set! convolve_px (quot convolve_kw 2)) (set! convolve_out []) (set! convolve_y 0) (while (< convolve_y convolve_h) (do (set! convolve_row []) (set! convolve_x 0) (while (< convolve_x convolve_w) (do (set! convolve_s 0.0) (set! convolve_ky 0) (while (< convolve_ky convolve_kh) (do (set! convolve_kx 0) (while (< convolve_kx convolve_kw) (do (set! convolve_iy (- (+ convolve_y convolve_ky) convolve_py)) (set! convolve_ix (- (+ convolve_x convolve_kx) convolve_px)) (when (and (and (and (>= convolve_iy 0) (< convolve_iy convolve_h)) (>= convolve_ix 0)) (< convolve_ix convolve_w)) (set! convolve_s (+ convolve_s (* (nth (nth convolve_img convolve_iy) convolve_ix) (nth (nth convolve_ker convolve_ky) convolve_kx))))) (set! convolve_kx (+ convolve_kx 1)))) (set! convolve_ky (+ convolve_ky 1)))) (set! convolve_row (conj convolve_row convolve_s)) (set! convolve_x (+ convolve_x 1)))) (set! convolve_out (conj convolve_out convolve_row)) (set! convolve_y (+ convolve_y 1)))) (throw (ex-info "return" {:v convolve_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn horn_schunck [horn_schunck_image0 horn_schunck_image1 horn_schunck_num_iter horn_schunck_alpha]
  (binding [horn_schunck_au nil horn_schunck_av nil horn_schunck_avg_u nil horn_schunck_avg_v nil horn_schunck_denom nil horn_schunck_dt nil horn_schunck_dt1 nil horn_schunck_dt2 nil horn_schunck_dx nil horn_schunck_dx1 nil horn_schunck_dx2 nil horn_schunck_dy nil horn_schunck_dy1 nil horn_schunck_dy2 nil horn_schunck_h nil horn_schunck_it nil horn_schunck_kernel_t nil horn_schunck_kernel_x nil horn_schunck_kernel_y nil horn_schunck_laplacian nil horn_schunck_numer nil horn_schunck_u nil horn_schunck_upd nil horn_schunck_v nil horn_schunck_w nil horn_schunck_warped nil horn_schunck_x nil horn_schunck_y nil] (try (do (set! horn_schunck_h (count horn_schunck_image0)) (set! horn_schunck_w (count (nth horn_schunck_image0 0))) (set! horn_schunck_u (zeros horn_schunck_h horn_schunck_w)) (set! horn_schunck_v (zeros horn_schunck_h horn_schunck_w)) (set! horn_schunck_kernel_x [[(- 0.25) 0.25] [(- 0.25) 0.25]]) (set! horn_schunck_kernel_y [[(- 0.25) (- 0.25)] [0.25 0.25]]) (set! horn_schunck_kernel_t [[0.25 0.25] [0.25 0.25]]) (set! horn_schunck_laplacian [[0.0833333333333 0.166666666667 0.0833333333333] [0.166666666667 0.0 0.166666666667] [0.0833333333333 0.166666666667 0.0833333333333]]) (set! horn_schunck_it 0) (while (< horn_schunck_it horn_schunck_num_iter) (do (set! horn_schunck_warped (warp horn_schunck_image0 horn_schunck_u horn_schunck_v)) (set! horn_schunck_dx1 (convolve horn_schunck_warped horn_schunck_kernel_x)) (set! horn_schunck_dx2 (convolve horn_schunck_image1 horn_schunck_kernel_x)) (set! horn_schunck_dy1 (convolve horn_schunck_warped horn_schunck_kernel_y)) (set! horn_schunck_dy2 (convolve horn_schunck_image1 horn_schunck_kernel_y)) (set! horn_schunck_dt1 (convolve horn_schunck_warped horn_schunck_kernel_t)) (set! horn_schunck_dt2 (convolve horn_schunck_image1 horn_schunck_kernel_t)) (set! horn_schunck_avg_u (convolve horn_schunck_u horn_schunck_laplacian)) (set! horn_schunck_avg_v (convolve horn_schunck_v horn_schunck_laplacian)) (set! horn_schunck_y 0) (while (< horn_schunck_y horn_schunck_h) (do (set! horn_schunck_x 0) (while (< horn_schunck_x horn_schunck_w) (do (set! horn_schunck_dx (+ (nth (nth horn_schunck_dx1 horn_schunck_y) horn_schunck_x) (nth (nth horn_schunck_dx2 horn_schunck_y) horn_schunck_x))) (set! horn_schunck_dy (+ (nth (nth horn_schunck_dy1 horn_schunck_y) horn_schunck_x) (nth (nth horn_schunck_dy2 horn_schunck_y) horn_schunck_x))) (set! horn_schunck_dt (- (nth (nth horn_schunck_dt1 horn_schunck_y) horn_schunck_x) (nth (nth horn_schunck_dt2 horn_schunck_y) horn_schunck_x))) (set! horn_schunck_au (nth (nth horn_schunck_avg_u horn_schunck_y) horn_schunck_x)) (set! horn_schunck_av (nth (nth horn_schunck_avg_v horn_schunck_y) horn_schunck_x)) (set! horn_schunck_numer (+ (+ (* horn_schunck_dx horn_schunck_au) (* horn_schunck_dy horn_schunck_av)) horn_schunck_dt)) (set! horn_schunck_denom (+ (+ (* horn_schunck_alpha horn_schunck_alpha) (* horn_schunck_dx horn_schunck_dx)) (* horn_schunck_dy horn_schunck_dy))) (set! horn_schunck_upd (/ horn_schunck_numer horn_schunck_denom)) (set! horn_schunck_u (assoc-in horn_schunck_u [horn_schunck_y horn_schunck_x] (- horn_schunck_au (* horn_schunck_dx horn_schunck_upd)))) (set! horn_schunck_v (assoc-in horn_schunck_v [horn_schunck_y horn_schunck_x] (- horn_schunck_av (* horn_schunck_dy horn_schunck_upd)))) (set! horn_schunck_x (+ horn_schunck_x 1)))) (set! horn_schunck_y (+ horn_schunck_y 1)))) (set! horn_schunck_it (+ horn_schunck_it 1)))) (throw (ex-info "return" {:v [horn_schunck_u horn_schunck_v]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix [print_matrix_mat]
  (binding [print_matrix_line nil print_matrix_row nil print_matrix_x nil print_matrix_y nil] (do (set! print_matrix_y 0) (while (< print_matrix_y (count print_matrix_mat)) (do (set! print_matrix_row (nth print_matrix_mat print_matrix_y)) (set! print_matrix_x 0) (set! print_matrix_line "") (while (< print_matrix_x (count print_matrix_row)) (do (set! print_matrix_line (str print_matrix_line (str (round_int (nth print_matrix_row print_matrix_x))))) (when (< (+ print_matrix_x 1) (count print_matrix_row)) (set! print_matrix_line (str print_matrix_line " "))) (set! print_matrix_x (+ print_matrix_x 1)))) (println print_matrix_line) (set! print_matrix_y (+ print_matrix_y 1)))))))

(defn main []
  (binding [main_flows nil main_image0 nil main_image1 nil main_u nil main_v nil] (do (set! main_image0 [[0.0 0.0 2.0] [0.0 0.0 2.0]]) (set! main_image1 [[0.0 2.0 0.0] [0.0 2.0 0.0]]) (set! main_flows (horn_schunck main_image0 main_image1 20 0.1)) (set! main_u (nth main_flows 0)) (set! main_v (nth main_flows 1)) (print_matrix main_u) (println "---") (print_matrix main_v))))

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
