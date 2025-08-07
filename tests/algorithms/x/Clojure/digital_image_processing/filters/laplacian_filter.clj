(ns main (:refer-clojure :exclude [make_matrix my_laplacian]))

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

(declare make_matrix my_laplacian)

(def ^:dynamic main_c nil)

(def ^:dynamic main_r nil)

(def ^:dynamic main_row_str nil)

(def ^:dynamic make_matrix_i nil)

(def ^:dynamic make_matrix_j nil)

(def ^:dynamic make_matrix_result nil)

(def ^:dynamic make_matrix_row nil)

(def ^:dynamic my_laplacian_cols nil)

(def ^:dynamic my_laplacian_i nil)

(def ^:dynamic my_laplacian_ii nil)

(def ^:dynamic my_laplacian_j nil)

(def ^:dynamic my_laplacian_jj nil)

(def ^:dynamic my_laplacian_k nil)

(def ^:dynamic my_laplacian_kernel nil)

(def ^:dynamic my_laplacian_ki nil)

(def ^:dynamic my_laplacian_kj nil)

(def ^:dynamic my_laplacian_output nil)

(def ^:dynamic my_laplacian_pad nil)

(def ^:dynamic my_laplacian_rows nil)

(def ^:dynamic my_laplacian_sum nil)

(def ^:dynamic my_laplacian_val nil)

(defn make_matrix [make_matrix_rows make_matrix_cols make_matrix_value]
  (binding [make_matrix_i nil make_matrix_j nil make_matrix_result nil make_matrix_row nil] (try (do (set! make_matrix_result []) (set! make_matrix_i 0) (while (< make_matrix_i make_matrix_rows) (do (set! make_matrix_row []) (set! make_matrix_j 0) (while (< make_matrix_j make_matrix_cols) (do (set! make_matrix_row (conj make_matrix_row make_matrix_value)) (set! make_matrix_j (+ make_matrix_j 1)))) (set! make_matrix_result (conj make_matrix_result make_matrix_row)) (set! make_matrix_i (+ make_matrix_i 1)))) (throw (ex-info "return" {:v make_matrix_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn my_laplacian [my_laplacian_src my_laplacian_ksize]
  (binding [my_laplacian_cols nil my_laplacian_i nil my_laplacian_ii nil my_laplacian_j nil my_laplacian_jj nil my_laplacian_k nil my_laplacian_kernel nil my_laplacian_ki nil my_laplacian_kj nil my_laplacian_output nil my_laplacian_pad nil my_laplacian_rows nil my_laplacian_sum nil my_laplacian_val nil] (try (do (set! my_laplacian_kernel []) (if (= my_laplacian_ksize 1) (set! my_laplacian_kernel [[0 (- 1) 0] [(- 1) 4 (- 1)] [0 (- 1) 0]]) (if (= my_laplacian_ksize 3) (set! my_laplacian_kernel [[0 1 0] [1 (- 4) 1] [0 1 0]]) (if (= my_laplacian_ksize 5) (set! my_laplacian_kernel [[0 0 (- 1) 0 0] [0 (- 1) (- 2) (- 1) 0] [(- 1) (- 2) 16 (- 2) (- 1)] [0 (- 1) (- 2) (- 1) 0] [0 0 (- 1) 0 0]]) (if (= my_laplacian_ksize 7) (set! my_laplacian_kernel [[0 0 0 (- 1) 0 0 0] [0 0 (- 2) (- 3) (- 2) 0 0] [0 (- 2) (- 7) (- 10) (- 7) (- 2) 0] [(- 1) (- 3) (- 10) 68 (- 10) (- 3) (- 1)] [0 (- 2) (- 7) (- 10) (- 7) (- 2) 0] [0 0 (- 2) (- 3) (- 2) 0 0] [0 0 0 (- 1) 0 0 0]]) (throw (Exception. "ksize must be in (1, 3, 5, 7)")))))) (set! my_laplacian_rows (count my_laplacian_src)) (set! my_laplacian_cols (count (nth my_laplacian_src 0))) (set! my_laplacian_k (count my_laplacian_kernel)) (set! my_laplacian_pad (quot my_laplacian_k 2)) (set! my_laplacian_output (make_matrix my_laplacian_rows my_laplacian_cols 0)) (set! my_laplacian_i 0) (while (< my_laplacian_i my_laplacian_rows) (do (set! my_laplacian_j 0) (while (< my_laplacian_j my_laplacian_cols) (do (set! my_laplacian_sum 0) (set! my_laplacian_ki 0) (while (< my_laplacian_ki my_laplacian_k) (do (set! my_laplacian_kj 0) (while (< my_laplacian_kj my_laplacian_k) (do (set! my_laplacian_ii (- (+ my_laplacian_i my_laplacian_ki) my_laplacian_pad)) (set! my_laplacian_jj (- (+ my_laplacian_j my_laplacian_kj) my_laplacian_pad)) (set! my_laplacian_val 0) (when (and (and (and (>= my_laplacian_ii 0) (< my_laplacian_ii my_laplacian_rows)) (>= my_laplacian_jj 0)) (< my_laplacian_jj my_laplacian_cols)) (set! my_laplacian_val (nth (nth my_laplacian_src my_laplacian_ii) my_laplacian_jj))) (set! my_laplacian_sum (+ my_laplacian_sum (* my_laplacian_val (nth (nth my_laplacian_kernel my_laplacian_ki) my_laplacian_kj)))) (set! my_laplacian_kj (+ my_laplacian_kj 1)))) (set! my_laplacian_ki (+ my_laplacian_ki 1)))) (set! my_laplacian_output (assoc-in my_laplacian_output [my_laplacian_i my_laplacian_j] my_laplacian_sum)) (set! my_laplacian_j (+ my_laplacian_j 1)))) (set! my_laplacian_i (+ my_laplacian_i 1)))) (throw (ex-info "return" {:v my_laplacian_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_image [[0 0 0 0 0] [0 10 10 10 0] [0 10 10 10 0] [0 10 10 10 0] [0 0 0 0 0]])

(def ^:dynamic main_result (my_laplacian main_image 3))

(def ^:dynamic main_r 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_r (count main_result)) (do (def ^:dynamic main_row_str "[") (def ^:dynamic main_c 0) (while (< main_c (count (nth main_result main_r))) (do (def main_row_str (str main_row_str (str (nth (nth main_result main_r) main_c)))) (when (< (+ main_c 1) (count (nth main_result main_r))) (def main_row_str (str main_row_str ", "))) (def main_c (+ main_c 1)))) (def main_row_str (str main_row_str "]")) (println main_row_str) (def main_r (+ main_r 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
