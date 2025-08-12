(ns main (:refer-clojure :exclude [check_matrix add subtract scalar_multiply multiply identity transpose main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare check_matrix add subtract scalar_multiply multiply identity transpose main)

(def ^:dynamic add_cols nil)

(def ^:dynamic add_i nil)

(def ^:dynamic add_j nil)

(def ^:dynamic add_result nil)

(def ^:dynamic add_row nil)

(def ^:dynamic add_rows nil)

(def ^:dynamic identity_i nil)

(def ^:dynamic identity_j nil)

(def ^:dynamic identity_result nil)

(def ^:dynamic identity_row nil)

(def ^:dynamic main_mat_a nil)

(def ^:dynamic main_mat_b nil)

(def ^:dynamic main_mat_c nil)

(def ^:dynamic multiply_cols nil)

(def ^:dynamic multiply_i nil)

(def ^:dynamic multiply_j nil)

(def ^:dynamic multiply_k nil)

(def ^:dynamic multiply_result nil)

(def ^:dynamic multiply_row nil)

(def ^:dynamic multiply_rows nil)

(def ^:dynamic multiply_sum nil)

(def ^:dynamic scalar_multiply_cols nil)

(def ^:dynamic scalar_multiply_i nil)

(def ^:dynamic scalar_multiply_j nil)

(def ^:dynamic scalar_multiply_result nil)

(def ^:dynamic scalar_multiply_row nil)

(def ^:dynamic scalar_multiply_rows nil)

(def ^:dynamic subtract_cols nil)

(def ^:dynamic subtract_i nil)

(def ^:dynamic subtract_j nil)

(def ^:dynamic subtract_result nil)

(def ^:dynamic subtract_row nil)

(def ^:dynamic subtract_rows nil)

(def ^:dynamic transpose_cols nil)

(def ^:dynamic transpose_i nil)

(def ^:dynamic transpose_j nil)

(def ^:dynamic transpose_result nil)

(def ^:dynamic transpose_row nil)

(def ^:dynamic transpose_rows nil)

(defn check_matrix [check_matrix_mat]
  (do (when (or (< (count check_matrix_mat) 2) (< (count (nth check_matrix_mat 0)) 2)) (throw (Exception. "Expected a matrix with at least 2x2 dimensions"))) check_matrix_mat))

(defn add [add_a add_b]
  (binding [add_cols nil add_i nil add_j nil add_result nil add_row nil add_rows nil] (try (do (check_matrix add_a) (check_matrix add_b) (when (or (not= (count add_a) (count add_b)) (not= (count (nth add_a 0)) (count (nth add_b 0)))) (throw (Exception. "Matrices must have the same dimensions"))) (set! add_rows (count add_a)) (set! add_cols (count (nth add_a 0))) (set! add_result []) (set! add_i 0) (while (< add_i add_rows) (do (set! add_row []) (set! add_j 0) (while (< add_j add_cols) (do (set! add_row (conj add_row (+ (nth (nth add_a add_i) add_j) (nth (nth add_b add_i) add_j)))) (set! add_j (+ add_j 1)))) (set! add_result (conj add_result add_row)) (set! add_i (+ add_i 1)))) (throw (ex-info "return" {:v add_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn subtract [subtract_a subtract_b]
  (binding [subtract_cols nil subtract_i nil subtract_j nil subtract_result nil subtract_row nil subtract_rows nil] (try (do (check_matrix subtract_a) (check_matrix subtract_b) (when (or (not= (count subtract_a) (count subtract_b)) (not= (count (nth subtract_a 0)) (count (nth subtract_b 0)))) (throw (Exception. "Matrices must have the same dimensions"))) (set! subtract_rows (count subtract_a)) (set! subtract_cols (count (nth subtract_a 0))) (set! subtract_result []) (set! subtract_i 0) (while (< subtract_i subtract_rows) (do (set! subtract_row []) (set! subtract_j 0) (while (< subtract_j subtract_cols) (do (set! subtract_row (conj subtract_row (- (nth (nth subtract_a subtract_i) subtract_j) (nth (nth subtract_b subtract_i) subtract_j)))) (set! subtract_j (+ subtract_j 1)))) (set! subtract_result (conj subtract_result subtract_row)) (set! subtract_i (+ subtract_i 1)))) (throw (ex-info "return" {:v subtract_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn scalar_multiply [scalar_multiply_a scalar_multiply_s]
  (binding [scalar_multiply_cols nil scalar_multiply_i nil scalar_multiply_j nil scalar_multiply_result nil scalar_multiply_row nil scalar_multiply_rows nil] (try (do (check_matrix scalar_multiply_a) (set! scalar_multiply_rows (count scalar_multiply_a)) (set! scalar_multiply_cols (count (nth scalar_multiply_a 0))) (set! scalar_multiply_result []) (set! scalar_multiply_i 0) (while (< scalar_multiply_i scalar_multiply_rows) (do (set! scalar_multiply_row []) (set! scalar_multiply_j 0) (while (< scalar_multiply_j scalar_multiply_cols) (do (set! scalar_multiply_row (conj scalar_multiply_row (* (nth (nth scalar_multiply_a scalar_multiply_i) scalar_multiply_j) scalar_multiply_s))) (set! scalar_multiply_j (+ scalar_multiply_j 1)))) (set! scalar_multiply_result (conj scalar_multiply_result scalar_multiply_row)) (set! scalar_multiply_i (+ scalar_multiply_i 1)))) (throw (ex-info "return" {:v scalar_multiply_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn multiply [multiply_a multiply_b]
  (binding [multiply_cols nil multiply_i nil multiply_j nil multiply_k nil multiply_result nil multiply_row nil multiply_rows nil multiply_sum nil] (try (do (check_matrix multiply_a) (check_matrix multiply_b) (when (not= (count (nth multiply_a 0)) (count multiply_b)) (throw (Exception. "Invalid dimensions for matrix multiplication"))) (set! multiply_rows (count multiply_a)) (set! multiply_cols (count (nth multiply_b 0))) (set! multiply_result []) (set! multiply_i 0) (while (< multiply_i multiply_rows) (do (set! multiply_row []) (set! multiply_j 0) (while (< multiply_j multiply_cols) (do (set! multiply_sum 0.0) (set! multiply_k 0) (while (< multiply_k (count multiply_b)) (do (set! multiply_sum (+ multiply_sum (* (nth (nth multiply_a multiply_i) multiply_k) (nth (nth multiply_b multiply_k) multiply_j)))) (set! multiply_k (+ multiply_k 1)))) (set! multiply_row (conj multiply_row multiply_sum)) (set! multiply_j (+ multiply_j 1)))) (set! multiply_result (conj multiply_result multiply_row)) (set! multiply_i (+ multiply_i 1)))) (throw (ex-info "return" {:v multiply_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn identity [identity_n]
  (binding [identity_i nil identity_j nil identity_result nil identity_row nil] (try (do (set! identity_result []) (set! identity_i 0) (while (< identity_i identity_n) (do (set! identity_row []) (set! identity_j 0) (while (< identity_j identity_n) (do (if (= identity_i identity_j) (set! identity_row (conj identity_row 1.0)) (set! identity_row (conj identity_row 0.0))) (set! identity_j (+ identity_j 1)))) (set! identity_result (conj identity_result identity_row)) (set! identity_i (+ identity_i 1)))) (throw (ex-info "return" {:v identity_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn transpose [transpose_a]
  (binding [transpose_cols nil transpose_i nil transpose_j nil transpose_result nil transpose_row nil transpose_rows nil] (try (do (check_matrix transpose_a) (set! transpose_rows (count transpose_a)) (set! transpose_cols (count (nth transpose_a 0))) (set! transpose_result []) (set! transpose_j 0) (while (< transpose_j transpose_cols) (do (set! transpose_row []) (set! transpose_i 0) (while (< transpose_i transpose_rows) (do (set! transpose_row (conj transpose_row (nth (nth transpose_a transpose_i) transpose_j))) (set! transpose_i (+ transpose_i 1)))) (set! transpose_result (conj transpose_result transpose_row)) (set! transpose_j (+ transpose_j 1)))) (throw (ex-info "return" {:v transpose_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_mat_a nil main_mat_b nil main_mat_c nil] (do (set! main_mat_a [[12.0 10.0] [3.0 9.0]]) (set! main_mat_b [[3.0 4.0] [7.0 4.0]]) (set! main_mat_c [[3.0 0.0 2.0] [2.0 0.0 (- 2.0)] [0.0 1.0 1.0]]) (println (str (add main_mat_a main_mat_b))) (println (str (subtract main_mat_a main_mat_b))) (println (str (multiply main_mat_a main_mat_b))) (println (str (scalar_multiply main_mat_a 3.5))) (println (str (identity 5))) (println (str (transpose main_mat_c))))))

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
