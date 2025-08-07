(ns main (:refer-clojure :exclude [pad_edge im2col flatten dot img_convolve print_matrix]))

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

(declare pad_edge im2col flatten dot img_convolve print_matrix)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_sum nil)

(def ^:dynamic flatten_i nil)

(def ^:dynamic flatten_j nil)

(def ^:dynamic flatten_out nil)

(def ^:dynamic im2col_bi nil)

(def ^:dynamic im2col_bj nil)

(def ^:dynamic im2col_cols nil)

(def ^:dynamic im2col_dst_height nil)

(def ^:dynamic im2col_dst_width nil)

(def ^:dynamic im2col_i nil)

(def ^:dynamic im2col_image_array nil)

(def ^:dynamic im2col_j nil)

(def ^:dynamic im2col_rows nil)

(def ^:dynamic im2col_window nil)

(def ^:dynamic img_convolve_dst nil)

(def ^:dynamic img_convolve_height nil)

(def ^:dynamic img_convolve_i nil)

(def ^:dynamic img_convolve_idx nil)

(def ^:dynamic img_convolve_image_array nil)

(def ^:dynamic img_convolve_j nil)

(def ^:dynamic img_convolve_k_size nil)

(def ^:dynamic img_convolve_kernel_flat nil)

(def ^:dynamic img_convolve_pad_size nil)

(def ^:dynamic img_convolve_padded nil)

(def ^:dynamic img_convolve_row nil)

(def ^:dynamic img_convolve_val nil)

(def ^:dynamic img_convolve_width nil)

(def ^:dynamic pad_edge_height nil)

(def ^:dynamic pad_edge_i nil)

(def ^:dynamic pad_edge_j nil)

(def ^:dynamic pad_edge_new_height nil)

(def ^:dynamic pad_edge_new_width nil)

(def ^:dynamic pad_edge_padded nil)

(def ^:dynamic pad_edge_row nil)

(def ^:dynamic pad_edge_src_i nil)

(def ^:dynamic pad_edge_src_j nil)

(def ^:dynamic pad_edge_width nil)

(def ^:dynamic print_matrix_i nil)

(def ^:dynamic print_matrix_j nil)

(def ^:dynamic print_matrix_line nil)

(defn pad_edge [pad_edge_image pad_edge_pad_size]
  (binding [pad_edge_height nil pad_edge_i nil pad_edge_j nil pad_edge_new_height nil pad_edge_new_width nil pad_edge_padded nil pad_edge_row nil pad_edge_src_i nil pad_edge_src_j nil pad_edge_width nil] (try (do (set! pad_edge_height (count pad_edge_image)) (set! pad_edge_width (count (nth pad_edge_image 0))) (set! pad_edge_new_height (+ pad_edge_height (* pad_edge_pad_size 2))) (set! pad_edge_new_width (+ pad_edge_width (* pad_edge_pad_size 2))) (set! pad_edge_padded []) (set! pad_edge_i 0) (while (< pad_edge_i pad_edge_new_height) (do (set! pad_edge_row []) (set! pad_edge_src_i pad_edge_i) (when (< pad_edge_src_i pad_edge_pad_size) (set! pad_edge_src_i 0)) (if (>= pad_edge_src_i (+ pad_edge_height pad_edge_pad_size)) (set! pad_edge_src_i (- pad_edge_height 1)) (set! pad_edge_src_i (- pad_edge_src_i pad_edge_pad_size))) (set! pad_edge_j 0) (while (< pad_edge_j pad_edge_new_width) (do (set! pad_edge_src_j pad_edge_j) (when (< pad_edge_src_j pad_edge_pad_size) (set! pad_edge_src_j 0)) (if (>= pad_edge_src_j (+ pad_edge_width pad_edge_pad_size)) (set! pad_edge_src_j (- pad_edge_width 1)) (set! pad_edge_src_j (- pad_edge_src_j pad_edge_pad_size))) (set! pad_edge_row (conj pad_edge_row (nth (nth pad_edge_image pad_edge_src_i) pad_edge_src_j))) (set! pad_edge_j (+ pad_edge_j 1)))) (set! pad_edge_padded (conj pad_edge_padded pad_edge_row)) (set! pad_edge_i (+ pad_edge_i 1)))) (throw (ex-info "return" {:v pad_edge_padded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn im2col [im2col_image im2col_block_h im2col_block_w]
  (binding [im2col_bi nil im2col_bj nil im2col_cols nil im2col_dst_height nil im2col_dst_width nil im2col_i nil im2col_image_array nil im2col_j nil im2col_rows nil im2col_window nil] (try (do (set! im2col_rows (count im2col_image)) (set! im2col_cols (count (nth im2col_image 0))) (set! im2col_dst_height (+ (- im2col_rows im2col_block_h) 1)) (set! im2col_dst_width (+ (- im2col_cols im2col_block_w) 1)) (set! im2col_image_array []) (set! im2col_i 0) (while (< im2col_i im2col_dst_height) (do (set! im2col_j 0) (while (< im2col_j im2col_dst_width) (do (set! im2col_window []) (set! im2col_bi 0) (while (< im2col_bi im2col_block_h) (do (set! im2col_bj 0) (while (< im2col_bj im2col_block_w) (do (set! im2col_window (conj im2col_window (nth (nth im2col_image (+ im2col_i im2col_bi)) (+ im2col_j im2col_bj)))) (set! im2col_bj (+ im2col_bj 1)))) (set! im2col_bi (+ im2col_bi 1)))) (set! im2col_image_array (conj im2col_image_array im2col_window)) (set! im2col_j (+ im2col_j 1)))) (set! im2col_i (+ im2col_i 1)))) (throw (ex-info "return" {:v im2col_image_array}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flatten [flatten_matrix]
  (binding [flatten_i nil flatten_j nil flatten_out nil] (try (do (set! flatten_out []) (set! flatten_i 0) (while (< flatten_i (count flatten_matrix)) (do (set! flatten_j 0) (while (< flatten_j (count (nth flatten_matrix flatten_i))) (do (set! flatten_out (conj flatten_out (nth (nth flatten_matrix flatten_i) flatten_j))) (set! flatten_j (+ flatten_j 1)))) (set! flatten_i (+ flatten_i 1)))) (throw (ex-info "return" {:v flatten_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_sum nil] (try (do (set! dot_sum 0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_sum (+ dot_sum (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn img_convolve [img_convolve_image img_convolve_kernel]
  (binding [img_convolve_dst nil img_convolve_height nil img_convolve_i nil img_convolve_idx nil img_convolve_image_array nil img_convolve_j nil img_convolve_k_size nil img_convolve_kernel_flat nil img_convolve_pad_size nil img_convolve_padded nil img_convolve_row nil img_convolve_val nil img_convolve_width nil] (try (do (set! img_convolve_height (count img_convolve_image)) (set! img_convolve_width (count (nth img_convolve_image 0))) (set! img_convolve_k_size (count img_convolve_kernel)) (set! img_convolve_pad_size (quot img_convolve_k_size 2)) (set! img_convolve_padded (pad_edge img_convolve_image img_convolve_pad_size)) (set! img_convolve_image_array (im2col img_convolve_padded img_convolve_k_size img_convolve_k_size)) (set! img_convolve_kernel_flat (flatten img_convolve_kernel)) (set! img_convolve_dst []) (set! img_convolve_idx 0) (set! img_convolve_i 0) (while (< img_convolve_i img_convolve_height) (do (set! img_convolve_row []) (set! img_convolve_j 0) (while (< img_convolve_j img_convolve_width) (do (set! img_convolve_val (dot (nth img_convolve_image_array img_convolve_idx) img_convolve_kernel_flat)) (set! img_convolve_row (conj img_convolve_row img_convolve_val)) (set! img_convolve_idx (+ img_convolve_idx 1)) (set! img_convolve_j (+ img_convolve_j 1)))) (set! img_convolve_dst (conj img_convolve_dst img_convolve_row)) (set! img_convolve_i (+ img_convolve_i 1)))) (throw (ex-info "return" {:v img_convolve_dst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix [print_matrix_m]
  (binding [print_matrix_i nil print_matrix_j nil print_matrix_line nil] (do (set! print_matrix_i 0) (while (< print_matrix_i (count print_matrix_m)) (do (set! print_matrix_line "") (set! print_matrix_j 0) (while (< print_matrix_j (count (nth print_matrix_m print_matrix_i))) (do (when (> print_matrix_j 0) (set! print_matrix_line (str print_matrix_line " "))) (set! print_matrix_line (str print_matrix_line (str (nth (nth print_matrix_m print_matrix_i) print_matrix_j)))) (set! print_matrix_j (+ print_matrix_j 1)))) (println print_matrix_line) (set! print_matrix_i (+ print_matrix_i 1)))))))

(def ^:dynamic main_image [[1 2 3 0 0] [4 5 6 0 0] [7 8 9 0 0] [0 0 0 0 0] [0 0 0 0 0]])

(def ^:dynamic main_laplace_kernel [[0 1 0] [1 (- 4) 1] [0 1 0]])

(def ^:dynamic main_result (img_convolve main_image main_laplace_kernel))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_matrix main_result)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
