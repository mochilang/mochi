(ns main (:refer-clojure :exclude [lu_decomposition print_matrix]))

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

(declare lu_decomposition print_matrix)

(def ^:dynamic lu_decomposition_i nil)

(def ^:dynamic lu_decomposition_j nil)

(def ^:dynamic lu_decomposition_j1 nil)

(def ^:dynamic lu_decomposition_j2 nil)

(def ^:dynamic lu_decomposition_k nil)

(def ^:dynamic lu_decomposition_k2 nil)

(def ^:dynamic lu_decomposition_lower nil)

(def ^:dynamic lu_decomposition_lrow nil)

(def ^:dynamic lu_decomposition_m nil)

(def ^:dynamic lu_decomposition_n nil)

(def ^:dynamic lu_decomposition_total nil)

(def ^:dynamic lu_decomposition_total2 nil)

(def ^:dynamic lu_decomposition_upper nil)

(def ^:dynamic lu_decomposition_urow nil)

(def ^:dynamic print_matrix_i nil)

(def ^:dynamic print_matrix_j nil)

(def ^:dynamic print_matrix_line nil)

(defn lu_decomposition [lu_decomposition_mat]
  (binding [lu_decomposition_i nil lu_decomposition_j nil lu_decomposition_j1 nil lu_decomposition_j2 nil lu_decomposition_k nil lu_decomposition_k2 nil lu_decomposition_lower nil lu_decomposition_lrow nil lu_decomposition_m nil lu_decomposition_n nil lu_decomposition_total nil lu_decomposition_total2 nil lu_decomposition_upper nil lu_decomposition_urow nil] (try (do (set! lu_decomposition_n (count lu_decomposition_mat)) (when (= lu_decomposition_n 0) (throw (ex-info "return" {:v {:lower [] :upper []}}))) (set! lu_decomposition_m (count (nth lu_decomposition_mat 0))) (when (not= lu_decomposition_n lu_decomposition_m) (throw (Exception. "Matrix must be square"))) (set! lu_decomposition_lower []) (set! lu_decomposition_upper []) (set! lu_decomposition_i 0) (while (< lu_decomposition_i lu_decomposition_n) (do (set! lu_decomposition_lrow []) (set! lu_decomposition_urow []) (set! lu_decomposition_j 0) (while (< lu_decomposition_j lu_decomposition_n) (do (set! lu_decomposition_lrow (conj lu_decomposition_lrow 0.0)) (set! lu_decomposition_urow (conj lu_decomposition_urow 0.0)) (set! lu_decomposition_j (+ lu_decomposition_j 1)))) (set! lu_decomposition_lower (conj lu_decomposition_lower lu_decomposition_lrow)) (set! lu_decomposition_upper (conj lu_decomposition_upper lu_decomposition_urow)) (set! lu_decomposition_i (+ lu_decomposition_i 1)))) (set! lu_decomposition_i 0) (while (< lu_decomposition_i lu_decomposition_n) (do (set! lu_decomposition_j1 0) (while (< lu_decomposition_j1 lu_decomposition_i) (do (set! lu_decomposition_total 0.0) (set! lu_decomposition_k 0) (while (< lu_decomposition_k lu_decomposition_i) (do (set! lu_decomposition_total (+ lu_decomposition_total (* (nth (nth lu_decomposition_lower lu_decomposition_i) lu_decomposition_k) (nth (nth lu_decomposition_upper lu_decomposition_k) lu_decomposition_j1)))) (set! lu_decomposition_k (+ lu_decomposition_k 1)))) (when (= (nth (nth lu_decomposition_upper lu_decomposition_j1) lu_decomposition_j1) 0.0) (throw (Exception. "No LU decomposition exists"))) (set! lu_decomposition_lower (assoc-in lu_decomposition_lower [lu_decomposition_i lu_decomposition_j1] (quot (- (nth (nth lu_decomposition_mat lu_decomposition_i) lu_decomposition_j1) lu_decomposition_total) (nth (nth lu_decomposition_upper lu_decomposition_j1) lu_decomposition_j1)))) (set! lu_decomposition_j1 (+ lu_decomposition_j1 1)))) (set! lu_decomposition_lower (assoc-in lu_decomposition_lower [lu_decomposition_i lu_decomposition_i] 1.0)) (set! lu_decomposition_j2 lu_decomposition_i) (while (< lu_decomposition_j2 lu_decomposition_n) (do (set! lu_decomposition_total2 0.0) (set! lu_decomposition_k2 0) (while (< lu_decomposition_k2 lu_decomposition_i) (do (set! lu_decomposition_total2 (+ lu_decomposition_total2 (* (nth (nth lu_decomposition_lower lu_decomposition_i) lu_decomposition_k2) (nth (nth lu_decomposition_upper lu_decomposition_k2) lu_decomposition_j2)))) (set! lu_decomposition_k2 (+ lu_decomposition_k2 1)))) (set! lu_decomposition_upper (assoc-in lu_decomposition_upper [lu_decomposition_i lu_decomposition_j2] (- (nth (nth lu_decomposition_mat lu_decomposition_i) lu_decomposition_j2) lu_decomposition_total2))) (set! lu_decomposition_j2 (+ lu_decomposition_j2 1)))) (set! lu_decomposition_i (+ lu_decomposition_i 1)))) (throw (ex-info "return" {:v {:lower lu_decomposition_lower :upper lu_decomposition_upper}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_matrix [print_matrix_mat]
  (binding [print_matrix_i nil print_matrix_j nil print_matrix_line nil] (do (set! print_matrix_i 0) (while (< print_matrix_i (count print_matrix_mat)) (do (set! print_matrix_line "") (set! print_matrix_j 0) (while (< print_matrix_j (count (nth print_matrix_mat print_matrix_i))) (do (set! print_matrix_line (str print_matrix_line (str (nth (nth print_matrix_mat print_matrix_i) print_matrix_j)))) (when (< (+ print_matrix_j 1) (count (nth print_matrix_mat print_matrix_i))) (set! print_matrix_line (str print_matrix_line " "))) (set! print_matrix_j (+ print_matrix_j 1)))) (println print_matrix_line) (set! print_matrix_i (+ print_matrix_i 1)))) print_matrix_mat)))

(def ^:dynamic main_matrix [[2.0 (- 2.0) 1.0] [0.0 1.0 2.0] [5.0 3.0 1.0]])

(def ^:dynamic main_result (lu_decomposition main_matrix))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_matrix (:lower main_result))
      (print_matrix (:upper main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
