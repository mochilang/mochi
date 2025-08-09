(ns main (:refer-clojure :exclude [absf strictly_diagonally_dominant jacobi_iteration_method]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare absf strictly_diagonally_dominant jacobi_iteration_method)

(def ^:dynamic jacobi_iteration_method_i nil)

(def ^:dynamic jacobi_iteration_method_j nil)

(def ^:dynamic jacobi_iteration_method_k nil)

(def ^:dynamic jacobi_iteration_method_n nil)

(def ^:dynamic jacobi_iteration_method_new_x nil)

(def ^:dynamic jacobi_iteration_method_r nil)

(def ^:dynamic jacobi_iteration_method_sum nil)

(def ^:dynamic jacobi_iteration_method_value nil)

(def ^:dynamic jacobi_iteration_method_x nil)

(def ^:dynamic strictly_diagonally_dominant_i nil)

(def ^:dynamic strictly_diagonally_dominant_j nil)

(def ^:dynamic strictly_diagonally_dominant_n nil)

(def ^:dynamic strictly_diagonally_dominant_sum nil)

(defn absf [absf_x]
  (try (throw (ex-info "return" {:v (if (< absf_x 0.0) (- absf_x) absf_x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn strictly_diagonally_dominant [strictly_diagonally_dominant_matrix]
  (binding [strictly_diagonally_dominant_i nil strictly_diagonally_dominant_j nil strictly_diagonally_dominant_n nil strictly_diagonally_dominant_sum nil] (try (do (set! strictly_diagonally_dominant_n (count strictly_diagonally_dominant_matrix)) (set! strictly_diagonally_dominant_i 0) (while (< strictly_diagonally_dominant_i strictly_diagonally_dominant_n) (do (set! strictly_diagonally_dominant_sum 0.0) (set! strictly_diagonally_dominant_j 0) (while (< strictly_diagonally_dominant_j strictly_diagonally_dominant_n) (do (when (not= strictly_diagonally_dominant_i strictly_diagonally_dominant_j) (set! strictly_diagonally_dominant_sum (+ strictly_diagonally_dominant_sum (absf (nth (nth strictly_diagonally_dominant_matrix strictly_diagonally_dominant_i) strictly_diagonally_dominant_j))))) (set! strictly_diagonally_dominant_j (+ strictly_diagonally_dominant_j 1)))) (when (<= (absf (nth (nth strictly_diagonally_dominant_matrix strictly_diagonally_dominant_i) strictly_diagonally_dominant_i)) strictly_diagonally_dominant_sum) (throw (Exception. "Coefficient matrix is not strictly diagonally dominant"))) (set! strictly_diagonally_dominant_i (+ strictly_diagonally_dominant_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn jacobi_iteration_method [jacobi_iteration_method_coefficient jacobi_iteration_method_constant jacobi_iteration_method_init_val jacobi_iteration_method_iterations]
  (binding [jacobi_iteration_method_i nil jacobi_iteration_method_j nil jacobi_iteration_method_k nil jacobi_iteration_method_n nil jacobi_iteration_method_new_x nil jacobi_iteration_method_r nil jacobi_iteration_method_sum nil jacobi_iteration_method_value nil jacobi_iteration_method_x nil] (try (do (set! jacobi_iteration_method_n (count jacobi_iteration_method_coefficient)) (when (= jacobi_iteration_method_n 0) (throw (Exception. "Coefficient matrix cannot be empty"))) (when (not= (count jacobi_iteration_method_constant) jacobi_iteration_method_n) (throw (Exception. "Constant vector length must equal number of rows in coefficient matrix"))) (when (not= (count jacobi_iteration_method_init_val) jacobi_iteration_method_n) (throw (Exception. "Initial values count must match matrix size"))) (set! jacobi_iteration_method_r 0) (while (< jacobi_iteration_method_r jacobi_iteration_method_n) (do (when (not= (count (nth jacobi_iteration_method_coefficient jacobi_iteration_method_r)) jacobi_iteration_method_n) (throw (Exception. "Coefficient matrix must be square"))) (set! jacobi_iteration_method_r (+ jacobi_iteration_method_r 1)))) (when (<= jacobi_iteration_method_iterations 0) (throw (Exception. "Iterations must be at least 1"))) (strictly_diagonally_dominant jacobi_iteration_method_coefficient) (set! jacobi_iteration_method_x jacobi_iteration_method_init_val) (set! jacobi_iteration_method_k 0) (while (< jacobi_iteration_method_k jacobi_iteration_method_iterations) (do (set! jacobi_iteration_method_new_x []) (set! jacobi_iteration_method_i 0) (while (< jacobi_iteration_method_i jacobi_iteration_method_n) (do (set! jacobi_iteration_method_sum 0.0) (set! jacobi_iteration_method_j 0) (while (< jacobi_iteration_method_j jacobi_iteration_method_n) (do (when (not= jacobi_iteration_method_i jacobi_iteration_method_j) (set! jacobi_iteration_method_sum (+ jacobi_iteration_method_sum (* (nth (nth jacobi_iteration_method_coefficient jacobi_iteration_method_i) jacobi_iteration_method_j) (nth jacobi_iteration_method_x jacobi_iteration_method_j))))) (set! jacobi_iteration_method_j (+ jacobi_iteration_method_j 1)))) (set! jacobi_iteration_method_value (quot (- (nth jacobi_iteration_method_constant jacobi_iteration_method_i) jacobi_iteration_method_sum) (nth (nth jacobi_iteration_method_coefficient jacobi_iteration_method_i) jacobi_iteration_method_i))) (set! jacobi_iteration_method_new_x (conj jacobi_iteration_method_new_x jacobi_iteration_method_value)) (set! jacobi_iteration_method_i (+ jacobi_iteration_method_i 1)))) (set! jacobi_iteration_method_x jacobi_iteration_method_new_x) (set! jacobi_iteration_method_k (+ jacobi_iteration_method_k 1)))) (throw (ex-info "return" {:v jacobi_iteration_method_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_coefficient [[4.0 1.0 1.0] [1.0 5.0 2.0] [1.0 2.0 4.0]])

(def ^:dynamic main_constant [2.0 (- 6.0) (- 4.0)])

(def ^:dynamic main_init_val [0.5 (- 0.5) (- 0.5)])

(def ^:dynamic main_iterations 3)

(def ^:dynamic main_result (jacobi_iteration_method main_coefficient main_constant main_init_val main_iterations))

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
