(ns main (:refer-clojure :exclude [conv2d relu_matrix max_pool2x2 flatten dense exp_approx sigmoid]))

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

(declare conv2d relu_matrix max_pool2x2 flatten dense exp_approx sigmoid)

(def ^:dynamic conv2d_cols nil)

(def ^:dynamic conv2d_i nil)

(def ^:dynamic conv2d_j nil)

(def ^:dynamic conv2d_k nil)

(def ^:dynamic conv2d_ki nil)

(def ^:dynamic conv2d_kj nil)

(def ^:dynamic conv2d_output nil)

(def ^:dynamic conv2d_row nil)

(def ^:dynamic conv2d_rows nil)

(def ^:dynamic conv2d_sum nil)

(def ^:dynamic dense_i nil)

(def ^:dynamic dense_s nil)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic flatten_res nil)

(def ^:dynamic max_pool2x2_cols nil)

(def ^:dynamic max_pool2x2_i nil)

(def ^:dynamic max_pool2x2_j nil)

(def ^:dynamic max_pool2x2_max_val nil)

(def ^:dynamic max_pool2x2_new_row nil)

(def ^:dynamic max_pool2x2_out nil)

(def ^:dynamic max_pool2x2_rows nil)

(def ^:dynamic relu_matrix_new_row nil)

(def ^:dynamic relu_matrix_out nil)

(defn conv2d [conv2d_image conv2d_kernel]
  (binding [conv2d_cols nil conv2d_i nil conv2d_j nil conv2d_k nil conv2d_ki nil conv2d_kj nil conv2d_output nil conv2d_row nil conv2d_rows nil conv2d_sum nil] (try (do (set! conv2d_rows (count conv2d_image)) (set! conv2d_cols (count (nth conv2d_image 0))) (set! conv2d_k (count conv2d_kernel)) (set! conv2d_output []) (set! conv2d_i 0) (while (<= conv2d_i (- conv2d_rows conv2d_k)) (do (set! conv2d_row []) (set! conv2d_j 0) (while (<= conv2d_j (- conv2d_cols conv2d_k)) (do (set! conv2d_sum 0.0) (set! conv2d_ki 0) (while (< conv2d_ki conv2d_k) (do (set! conv2d_kj 0) (while (< conv2d_kj conv2d_k) (do (set! conv2d_sum (+ conv2d_sum (* (nth (nth conv2d_image (+ conv2d_i conv2d_ki)) (+ conv2d_j conv2d_kj)) (nth (nth conv2d_kernel conv2d_ki) conv2d_kj)))) (set! conv2d_kj (+ conv2d_kj 1)))) (set! conv2d_ki (+ conv2d_ki 1)))) (set! conv2d_row (conj conv2d_row conv2d_sum)) (set! conv2d_j (+ conv2d_j 1)))) (set! conv2d_output (conj conv2d_output conv2d_row)) (set! conv2d_i (+ conv2d_i 1)))) (throw (ex-info "return" {:v conv2d_output}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn relu_matrix [relu_matrix_m]
  (binding [relu_matrix_new_row nil relu_matrix_out nil] (try (do (set! relu_matrix_out []) (doseq [row relu_matrix_m] (do (set! relu_matrix_new_row []) (doseq [v row] (if (> v 0.0) (set! relu_matrix_new_row (conj relu_matrix_new_row v)) (set! relu_matrix_new_row (conj relu_matrix_new_row 0.0)))) (set! relu_matrix_out (conj relu_matrix_out relu_matrix_new_row)))) (throw (ex-info "return" {:v relu_matrix_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_pool2x2 [max_pool2x2_m]
  (binding [max_pool2x2_cols nil max_pool2x2_i nil max_pool2x2_j nil max_pool2x2_max_val nil max_pool2x2_new_row nil max_pool2x2_out nil max_pool2x2_rows nil] (try (do (set! max_pool2x2_rows (count max_pool2x2_m)) (set! max_pool2x2_cols (count (nth max_pool2x2_m 0))) (set! max_pool2x2_out []) (set! max_pool2x2_i 0) (while (< max_pool2x2_i max_pool2x2_rows) (do (set! max_pool2x2_new_row []) (set! max_pool2x2_j 0) (while (< max_pool2x2_j max_pool2x2_cols) (do (set! max_pool2x2_max_val (nth (nth max_pool2x2_m max_pool2x2_i) max_pool2x2_j)) (when (> (nth (nth max_pool2x2_m max_pool2x2_i) (+ max_pool2x2_j 1)) max_pool2x2_max_val) (set! max_pool2x2_max_val (nth (nth max_pool2x2_m max_pool2x2_i) (+ max_pool2x2_j 1)))) (when (> (nth (nth max_pool2x2_m (+ max_pool2x2_i 1)) max_pool2x2_j) max_pool2x2_max_val) (set! max_pool2x2_max_val (nth (nth max_pool2x2_m (+ max_pool2x2_i 1)) max_pool2x2_j))) (when (> (nth (nth max_pool2x2_m (+ max_pool2x2_i 1)) (+ max_pool2x2_j 1)) max_pool2x2_max_val) (set! max_pool2x2_max_val (nth (nth max_pool2x2_m (+ max_pool2x2_i 1)) (+ max_pool2x2_j 1)))) (set! max_pool2x2_new_row (conj max_pool2x2_new_row max_pool2x2_max_val)) (set! max_pool2x2_j (+ max_pool2x2_j 2)))) (set! max_pool2x2_out (conj max_pool2x2_out max_pool2x2_new_row)) (set! max_pool2x2_i (+ max_pool2x2_i 2)))) (throw (ex-info "return" {:v max_pool2x2_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flatten [flatten_m]
  (binding [flatten_res nil] (try (do (set! flatten_res []) (doseq [row flatten_m] (doseq [v row] (set! flatten_res (conj flatten_res v)))) (throw (ex-info "return" {:v flatten_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dense [dense_inputs dense_weights dense_bias]
  (binding [dense_i nil dense_s nil] (try (do (set! dense_s dense_bias) (set! dense_i 0) (while (< dense_i (count dense_inputs)) (do (set! dense_s (+ dense_s (* (nth dense_inputs dense_i) (nth dense_weights dense_i)))) (set! dense_i (+ dense_i 1)))) (throw (ex-info "return" {:v dense_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_sum 1.0) (set! exp_approx_term 1.0) (set! exp_approx_i 1) (while (<= exp_approx_i 10) (do (set! exp_approx_term (/ (* exp_approx_term exp_approx_x) exp_approx_i)) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_i (+ exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_x]
  (try (throw (ex-info "return" {:v (/ 1.0 (+ 1.0 (exp_approx (- sigmoid_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_image [[0.0 1.0 1.0 0.0 0.0 0.0] [0.0 1.0 1.0 0.0 0.0 0.0] [0.0 0.0 1.0 1.0 0.0 0.0] [0.0 0.0 1.0 1.0 0.0 0.0] [0.0 0.0 0.0 0.0 0.0 0.0] [0.0 0.0 0.0 0.0 0.0 0.0]])

(def ^:dynamic main_kernel [[1.0 0.0 (- 1.0)] [1.0 0.0 (- 1.0)] [1.0 0.0 (- 1.0)]])

(def ^:dynamic main_conv (conv2d main_image main_kernel))

(def ^:dynamic main_activated (relu_matrix main_conv))

(def ^:dynamic main_pooled (max_pool2x2 main_activated))

(def ^:dynamic main_flat (flatten main_pooled))

(def ^:dynamic main_weights [0.5 (- 0.4) 0.3 0.1])

(def ^:dynamic main_bias 0.0)

(def ^:dynamic main_output (dense main_flat main_weights main_bias))

(def ^:dynamic main_probability (sigmoid main_output))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (if (>= main_probability 0.5) (println "Abnormality detected") (println "Normal"))
      (println "Probability:")
      (println main_probability)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
