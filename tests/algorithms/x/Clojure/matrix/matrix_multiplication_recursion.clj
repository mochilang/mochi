(ns main (:refer-clojure :exclude [is_square matrix_multiply multiply matrix_multiply_recursive]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare is_square matrix_multiply multiply matrix_multiply_recursive)

(def ^:dynamic is_square_i nil)

(def ^:dynamic is_square_n nil)

(def ^:dynamic matrix_multiply_cols nil)

(def ^:dynamic matrix_multiply_i nil)

(def ^:dynamic matrix_multiply_inner nil)

(def ^:dynamic matrix_multiply_j nil)

(def ^:dynamic matrix_multiply_k nil)

(def ^:dynamic matrix_multiply_recursive_i nil)

(def ^:dynamic matrix_multiply_recursive_j nil)

(def ^:dynamic matrix_multiply_recursive_m nil)

(def ^:dynamic matrix_multiply_recursive_n nil)

(def ^:dynamic matrix_multiply_recursive_result nil)

(def ^:dynamic matrix_multiply_recursive_row nil)

(def ^:dynamic matrix_multiply_result nil)

(def ^:dynamic matrix_multiply_row nil)

(def ^:dynamic matrix_multiply_rows nil)

(def ^:dynamic matrix_multiply_sum nil)

(def ^:dynamic multiply_result nil)

(defn is_square [is_square_matrix]
  (binding [is_square_i nil is_square_n nil] (try (do (set! is_square_n (count is_square_matrix)) (set! is_square_i 0) (while (< is_square_i is_square_n) (do (when (not= (count (nth is_square_matrix is_square_i)) is_square_n) (throw (ex-info "return" {:v false}))) (set! is_square_i (+ is_square_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_multiply [matrix_multiply_a matrix_multiply_b]
  (binding [matrix_multiply_cols nil matrix_multiply_i nil matrix_multiply_inner nil matrix_multiply_j nil matrix_multiply_k nil matrix_multiply_result nil matrix_multiply_row nil matrix_multiply_rows nil matrix_multiply_sum nil] (try (do (set! matrix_multiply_rows (count matrix_multiply_a)) (set! matrix_multiply_cols (count (nth matrix_multiply_b 0))) (set! matrix_multiply_inner (count matrix_multiply_b)) (set! matrix_multiply_result []) (set! matrix_multiply_i 0) (while (< matrix_multiply_i matrix_multiply_rows) (do (set! matrix_multiply_row []) (set! matrix_multiply_j 0) (while (< matrix_multiply_j matrix_multiply_cols) (do (set! matrix_multiply_sum 0) (set! matrix_multiply_k 0) (while (< matrix_multiply_k matrix_multiply_inner) (do (set! matrix_multiply_sum (+ matrix_multiply_sum (* (nth (nth matrix_multiply_a matrix_multiply_i) matrix_multiply_k) (nth (nth matrix_multiply_b matrix_multiply_k) matrix_multiply_j)))) (set! matrix_multiply_k (+ matrix_multiply_k 1)))) (set! matrix_multiply_row (conj matrix_multiply_row matrix_multiply_sum)) (set! matrix_multiply_j (+ matrix_multiply_j 1)))) (set! matrix_multiply_result (conj matrix_multiply_result matrix_multiply_row)) (set! matrix_multiply_i (+ matrix_multiply_i 1)))) (throw (ex-info "return" {:v matrix_multiply_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn multiply [multiply_i multiply_j multiply_k multiply_a multiply_b multiply_result_p multiply_n multiply_m]
  (binding [multiply_result multiply_result_p] (try (do (when (>= multiply_i multiply_n) (throw (ex-info "return" {:v nil}))) (when (>= multiply_j multiply_m) (do (let [__res (multiply (+ multiply_i 1) 0 0 multiply_a multiply_b multiply_result multiply_n multiply_m)] (do (set! multiply_result multiply_result) __res)) (throw (ex-info "return" {:v nil})))) (when (>= multiply_k (count multiply_b)) (do (let [__res (multiply multiply_i (+ multiply_j 1) 0 multiply_a multiply_b multiply_result multiply_n multiply_m)] (do (set! multiply_result multiply_result) __res)) (throw (ex-info "return" {:v nil})))) (set! multiply_result (assoc-in multiply_result [multiply_i multiply_j] (+ (nth (nth multiply_result multiply_i) multiply_j) (* (nth (nth multiply_a multiply_i) multiply_k) (nth (nth multiply_b multiply_k) multiply_j))))) (let [__res (multiply multiply_i multiply_j (+ multiply_k 1) multiply_a multiply_b multiply_result multiply_n multiply_m)] (do (set! multiply_result multiply_result) __res))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var multiply_result) (constantly multiply_result))))))

(defn matrix_multiply_recursive [matrix_multiply_recursive_a matrix_multiply_recursive_b]
  (binding [matrix_multiply_recursive_i nil matrix_multiply_recursive_j nil matrix_multiply_recursive_m nil matrix_multiply_recursive_n nil matrix_multiply_recursive_result nil matrix_multiply_recursive_row nil] (try (do (when (or (= (count matrix_multiply_recursive_a) 0) (= (count matrix_multiply_recursive_b) 0)) (throw (ex-info "return" {:v []}))) (when (or (or (not= (count matrix_multiply_recursive_a) (count matrix_multiply_recursive_b)) (not (is_square matrix_multiply_recursive_a))) (not (is_square matrix_multiply_recursive_b))) (throw (Exception. "Invalid matrix dimensions"))) (set! matrix_multiply_recursive_n (count matrix_multiply_recursive_a)) (set! matrix_multiply_recursive_m (count (nth matrix_multiply_recursive_b 0))) (set! matrix_multiply_recursive_result []) (set! matrix_multiply_recursive_i 0) (while (< matrix_multiply_recursive_i matrix_multiply_recursive_n) (do (set! matrix_multiply_recursive_row []) (set! matrix_multiply_recursive_j 0) (while (< matrix_multiply_recursive_j matrix_multiply_recursive_m) (do (set! matrix_multiply_recursive_row (conj matrix_multiply_recursive_row 0)) (set! matrix_multiply_recursive_j (+ matrix_multiply_recursive_j 1)))) (set! matrix_multiply_recursive_result (conj matrix_multiply_recursive_result matrix_multiply_recursive_row)) (set! matrix_multiply_recursive_i (+ matrix_multiply_recursive_i 1)))) (let [__res (multiply 0 0 0 matrix_multiply_recursive_a matrix_multiply_recursive_b matrix_multiply_recursive_result matrix_multiply_recursive_n matrix_multiply_recursive_m)] (do (set! matrix_multiply_recursive_result multiply_result) __res)) (throw (ex-info "return" {:v matrix_multiply_recursive_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_matrix_1_to_4 nil)

(def ^:dynamic main_matrix_5_to_8 nil)

(def ^:dynamic main_matrix_count_up nil)

(def ^:dynamic main_matrix_unordered nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_matrix_1_to_4) (constantly [[1 2] [3 4]]))
      (alter-var-root (var main_matrix_5_to_8) (constantly [[5 6] [7 8]]))
      (alter-var-root (var main_matrix_count_up) (constantly [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]))
      (alter-var-root (var main_matrix_unordered) (constantly [[5 8 1 2] [6 7 3 0] [4 5 9 1] [2 6 10 14]]))
      (println (matrix_multiply_recursive main_matrix_1_to_4 main_matrix_5_to_8))
      (println (matrix_multiply_recursive main_matrix_count_up main_matrix_unordered))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
