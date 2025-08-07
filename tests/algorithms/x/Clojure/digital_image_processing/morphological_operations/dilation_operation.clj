(ns main (:refer-clojure :exclude [rgb_to_gray gray_to_binary dilation print_float_matrix print_int_matrix main]))

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

(declare rgb_to_gray gray_to_binary dilation print_float_matrix print_int_matrix main)

(def ^:dynamic dilation_i nil)

(def ^:dynamic dilation_img_h nil)

(def ^:dynamic dilation_img_w nil)

(def ^:dynamic dilation_j nil)

(def ^:dynamic dilation_k_h nil)

(def ^:dynamic dilation_k_w nil)

(def ^:dynamic dilation_kx nil)

(def ^:dynamic dilation_ky nil)

(def ^:dynamic dilation_output nil)

(def ^:dynamic dilation_p_h nil)

(def ^:dynamic dilation_p_w nil)

(def ^:dynamic dilation_pad_h nil)

(def ^:dynamic dilation_pad_w nil)

(def ^:dynamic dilation_padded nil)

(def ^:dynamic dilation_row nil)

(def ^:dynamic dilation_sum nil)

(def ^:dynamic gray_to_binary_i nil)

(def ^:dynamic gray_to_binary_j nil)

(def ^:dynamic gray_to_binary_result nil)

(def ^:dynamic gray_to_binary_row nil)

(def ^:dynamic gray_to_binary_v nil)

(def ^:dynamic main_binary_image nil)

(def ^:dynamic main_gray_example nil)

(def ^:dynamic main_kernel nil)

(def ^:dynamic main_rgb_example nil)

(def ^:dynamic print_float_matrix_i nil)

(def ^:dynamic print_float_matrix_j nil)

(def ^:dynamic print_float_matrix_line nil)

(def ^:dynamic print_int_matrix_i nil)

(def ^:dynamic print_int_matrix_j nil)

(def ^:dynamic print_int_matrix_line nil)

(def ^:dynamic rgb_to_gray_b nil)

(def ^:dynamic rgb_to_gray_g nil)

(def ^:dynamic rgb_to_gray_gray nil)

(def ^:dynamic rgb_to_gray_i nil)

(def ^:dynamic rgb_to_gray_j nil)

(def ^:dynamic rgb_to_gray_r nil)

(def ^:dynamic rgb_to_gray_result nil)

(def ^:dynamic rgb_to_gray_row nil)

(defn rgb_to_gray [rgb_to_gray_rgb]
  (binding [rgb_to_gray_b nil rgb_to_gray_g nil rgb_to_gray_gray nil rgb_to_gray_i nil rgb_to_gray_j nil rgb_to_gray_r nil rgb_to_gray_result nil rgb_to_gray_row nil] (try (do (set! rgb_to_gray_result []) (set! rgb_to_gray_i 0) (while (< rgb_to_gray_i (count rgb_to_gray_rgb)) (do (set! rgb_to_gray_row []) (set! rgb_to_gray_j 0) (while (< rgb_to_gray_j (count (nth rgb_to_gray_rgb rgb_to_gray_i))) (do (set! rgb_to_gray_r (nth (nth (nth rgb_to_gray_rgb rgb_to_gray_i) rgb_to_gray_j) 0)) (set! rgb_to_gray_g (nth (nth (nth rgb_to_gray_rgb rgb_to_gray_i) rgb_to_gray_j) 1)) (set! rgb_to_gray_b (nth (nth (nth rgb_to_gray_rgb rgb_to_gray_i) rgb_to_gray_j) 2)) (set! rgb_to_gray_gray (+ (+ (* 0.2989 (* 1.0 rgb_to_gray_r)) (* 0.587 (* 1.0 rgb_to_gray_g))) (* 0.114 (* 1.0 rgb_to_gray_b)))) (set! rgb_to_gray_row (conj rgb_to_gray_row rgb_to_gray_gray)) (set! rgb_to_gray_j (+ rgb_to_gray_j 1)))) (set! rgb_to_gray_result (conj rgb_to_gray_result rgb_to_gray_row)) (set! rgb_to_gray_i (+ rgb_to_gray_i 1)))) (throw (ex-info "return" {:v rgb_to_gray_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gray_to_binary [gray_to_binary_gray]
  (binding [gray_to_binary_i nil gray_to_binary_j nil gray_to_binary_result nil gray_to_binary_row nil gray_to_binary_v nil] (try (do (set! gray_to_binary_result []) (set! gray_to_binary_i 0) (while (< gray_to_binary_i (count gray_to_binary_gray)) (do (set! gray_to_binary_row []) (set! gray_to_binary_j 0) (while (< gray_to_binary_j (count (nth gray_to_binary_gray gray_to_binary_i))) (do (set! gray_to_binary_v (nth (nth gray_to_binary_gray gray_to_binary_i) gray_to_binary_j)) (if (and (> gray_to_binary_v 127.0) (<= gray_to_binary_v 255.0)) (set! gray_to_binary_row (conj gray_to_binary_row 1)) (set! gray_to_binary_row (conj gray_to_binary_row 0))) (set! gray_to_binary_j (+ gray_to_binary_j 1)))) (set! gray_to_binary_result (conj gray_to_binary_result gray_to_binary_row)) (set! gray_to_binary_i (+ gray_to_binary_i 1)))) (throw (ex-info "return" {:v gray_to_binary_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dilation [dilation_image dilation_kernel]
  (binding [dilation_i nil dilation_img_h nil dilation_img_w nil dilation_j nil dilation_k_h nil dilation_k_w nil dilation_kx nil dilation_ky nil dilation_output nil dilation_p_h nil dilation_p_w nil dilation_pad_h nil dilation_pad_w nil dilation_padded nil dilation_row nil dilation_sum nil] (try (do (set! dilation_img_h (count dilation_image)) (set! dilation_img_w (count (nth dilation_image 0))) (set! dilation_k_h (count dilation_kernel)) (set! dilation_k_w (count (nth dilation_kernel 0))) (set! dilation_pad_h (quot dilation_k_h 2)) (set! dilation_pad_w (quot dilation_k_w 2)) (set! dilation_p_h (+ dilation_img_h (* 2 dilation_pad_h))) (set! dilation_p_w (+ dilation_img_w (* 2 dilation_pad_w))) (set! dilation_padded []) (set! dilation_i 0) (while (< dilation_i dilation_p_h) (do (set! dilation_row []) (set! dilation_j 0) (while (< dilation_j dilation_p_w) (do (set! dilation_row (conj dilation_row 0)) (set! dilation_j (+ dilation_j 1)))) (set! dilation_padded (conj dilation_padded dilation_row)) (set! dilation_i (+ dilation_i 1)))) (set! dilation_i 0) (while (< dilation_i dilation_img_h) (do (set! dilation_j 0) (while (< dilation_j dilation_img_w) (do (set! dilation_padded (assoc-in dilation_padded [(+ dilation_pad_h dilation_i) (+ dilation_pad_w dilation_j)] (nth (nth dilation_image dilation_i) dilation_j))) (set! dilation_j (+ dilation_j 1)))) (set! dilation_i (+ dilation_i 1)))) (set! dilation_output []) (set! dilation_i 0) (while (< dilation_i dilation_img_h) (do (set! dilation_row []) (set! dilation_j 0) (while (< dilation_j dilation_img_w) (do (set! dilation_sum 0) (set! dilation_ky 0) (while (< dilation_ky dilation_k_h) (do (set! dilation_kx 0) (while (< dilation_kx dilation_k_w) (do (when (= (nth (nth dilation_kernel dilation_ky) dilation_kx) 1) (set! dilation_sum (+ dilation_sum (nth (nth dilation_padded (+ dilation_i dilation_ky)) (+ dilation_j dilation_kx))))) (set! dilation_kx (+ dilation_kx 1)))) (set! dilation_ky (+ dilation_ky 1)))) (if (> dilation_sum 0) (set! dilation_row (conj dilation_row 1)) (set! dilation_row (conj dilation_row 0))) (set! dilation_j (+ dilation_j 1)))) (set! dilation_output (conj dilation_output dilation_row)) (set! dilation_i (+ dilation_i 1)))) (throw (ex-info "return" {:v dilation_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_float_matrix [print_float_matrix_mat]
  (binding [print_float_matrix_i nil print_float_matrix_j nil print_float_matrix_line nil] (do (set! print_float_matrix_i 0) (while (< print_float_matrix_i (count print_float_matrix_mat)) (do (set! print_float_matrix_line "") (set! print_float_matrix_j 0) (while (< print_float_matrix_j (count (nth print_float_matrix_mat print_float_matrix_i))) (do (set! print_float_matrix_line (str print_float_matrix_line (str (nth (nth print_float_matrix_mat print_float_matrix_i) print_float_matrix_j)))) (when (< print_float_matrix_j (- (count (nth print_float_matrix_mat print_float_matrix_i)) 1)) (set! print_float_matrix_line (str print_float_matrix_line " "))) (set! print_float_matrix_j (+ print_float_matrix_j 1)))) (println print_float_matrix_line) (set! print_float_matrix_i (+ print_float_matrix_i 1)))))))

(defn print_int_matrix [print_int_matrix_mat]
  (binding [print_int_matrix_i nil print_int_matrix_j nil print_int_matrix_line nil] (do (set! print_int_matrix_i 0) (while (< print_int_matrix_i (count print_int_matrix_mat)) (do (set! print_int_matrix_line "") (set! print_int_matrix_j 0) (while (< print_int_matrix_j (count (nth print_int_matrix_mat print_int_matrix_i))) (do (set! print_int_matrix_line (str print_int_matrix_line (str (nth (nth print_int_matrix_mat print_int_matrix_i) print_int_matrix_j)))) (when (< print_int_matrix_j (- (count (nth print_int_matrix_mat print_int_matrix_i)) 1)) (set! print_int_matrix_line (str print_int_matrix_line " "))) (set! print_int_matrix_j (+ print_int_matrix_j 1)))) (println print_int_matrix_line) (set! print_int_matrix_i (+ print_int_matrix_i 1)))))))

(defn main []
  (binding [main_binary_image nil main_gray_example nil main_kernel nil main_rgb_example nil] (do (set! main_rgb_example [[[127 255 0]]]) (print_float_matrix (rgb_to_gray main_rgb_example)) (set! main_gray_example [[26.0 255.0 14.0] [5.0 147.0 20.0] [1.0 200.0 0.0]]) (print_int_matrix (gray_to_binary main_gray_example)) (set! main_binary_image [[0 1 0] [0 1 0] [0 1 0]]) (set! main_kernel [[0 1 0] [1 1 1] [0 1 0]]) (print_int_matrix (dilation main_binary_image main_kernel)))))

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
