(ns main (:refer-clojure :exclude [make_matrix matrix_from_lists matrix_to_string matrix_add matrix_sub matrix_mul_scalar matrix_mul matrix_transpose sherman_morrison main]))

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

(declare make_matrix matrix_from_lists matrix_to_string matrix_add matrix_sub matrix_mul_scalar matrix_mul matrix_transpose sherman_morrison main)

(def ^:dynamic main_ainv nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_u nil)

(def ^:dynamic main_v nil)

(def ^:dynamic make_matrix_arr nil)

(def ^:dynamic make_matrix_c nil)

(def ^:dynamic make_matrix_r nil)

(def ^:dynamic make_matrix_row nil)

(def ^:dynamic matrix_add_i nil)

(def ^:dynamic matrix_add_j nil)

(def ^:dynamic matrix_add_res nil)

(def ^:dynamic matrix_add_row nil)

(def ^:dynamic matrix_from_lists_c nil)

(def ^:dynamic matrix_from_lists_r nil)

(def ^:dynamic matrix_mul_i nil)

(def ^:dynamic matrix_mul_j nil)

(def ^:dynamic matrix_mul_k nil)

(def ^:dynamic matrix_mul_res nil)

(def ^:dynamic matrix_mul_row nil)

(def ^:dynamic matrix_mul_scalar_i nil)

(def ^:dynamic matrix_mul_scalar_j nil)

(def ^:dynamic matrix_mul_scalar_res nil)

(def ^:dynamic matrix_mul_scalar_row nil)

(def ^:dynamic matrix_mul_sum nil)

(def ^:dynamic matrix_sub_i nil)

(def ^:dynamic matrix_sub_j nil)

(def ^:dynamic matrix_sub_res nil)

(def ^:dynamic matrix_sub_row nil)

(def ^:dynamic matrix_to_string_i nil)

(def ^:dynamic matrix_to_string_j nil)

(def ^:dynamic matrix_to_string_s nil)

(def ^:dynamic matrix_transpose_c nil)

(def ^:dynamic matrix_transpose_r nil)

(def ^:dynamic matrix_transpose_res nil)

(def ^:dynamic matrix_transpose_row nil)

(def ^:dynamic sherman_morrison_factor nil)

(def ^:dynamic sherman_morrison_numerator nil)

(def ^:dynamic sherman_morrison_scaled nil)

(def ^:dynamic sherman_morrison_term1 nil)

(def ^:dynamic sherman_morrison_term2 nil)

(def ^:dynamic sherman_morrison_vt nil)

(def ^:dynamic sherman_morrison_vu nil)

(defn make_matrix [make_matrix_rows make_matrix_cols make_matrix_value]
  (binding [make_matrix_arr nil make_matrix_c nil make_matrix_r nil make_matrix_row nil] (try (do (set! make_matrix_arr []) (set! make_matrix_r 0) (while (< make_matrix_r make_matrix_rows) (do (set! make_matrix_row []) (set! make_matrix_c 0) (while (< make_matrix_c make_matrix_cols) (do (set! make_matrix_row (conj make_matrix_row make_matrix_value)) (set! make_matrix_c (+ make_matrix_c 1)))) (set! make_matrix_arr (conj make_matrix_arr make_matrix_row)) (set! make_matrix_r (+ make_matrix_r 1)))) (throw (ex-info "return" {:v {:cols make_matrix_cols :data make_matrix_arr :rows make_matrix_rows}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_from_lists [matrix_from_lists_vals]
  (binding [matrix_from_lists_c nil matrix_from_lists_r nil] (try (do (set! matrix_from_lists_r (count matrix_from_lists_vals)) (set! matrix_from_lists_c (if (= matrix_from_lists_r 0) 0 (count (nth matrix_from_lists_vals 0)))) (throw (ex-info "return" {:v {:cols matrix_from_lists_c :data matrix_from_lists_vals :rows matrix_from_lists_r}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_to_string [matrix_to_string_m]
  (binding [matrix_to_string_i nil matrix_to_string_j nil matrix_to_string_s nil] (try (do (set! matrix_to_string_s "") (set! matrix_to_string_i 0) (while (< matrix_to_string_i (:rows matrix_to_string_m)) (do (set! matrix_to_string_s (str matrix_to_string_s "[")) (set! matrix_to_string_j 0) (while (< matrix_to_string_j (:cols matrix_to_string_m)) (do (set! matrix_to_string_s (str matrix_to_string_s (mochi_str (get (get (:data matrix_to_string_m) matrix_to_string_i) matrix_to_string_j)))) (when (< matrix_to_string_j (- (:cols matrix_to_string_m) 1)) (set! matrix_to_string_s (str matrix_to_string_s ", "))) (set! matrix_to_string_j (+ matrix_to_string_j 1)))) (set! matrix_to_string_s (str matrix_to_string_s "]")) (when (< matrix_to_string_i (- (:rows matrix_to_string_m) 1)) (set! matrix_to_string_s (str matrix_to_string_s "\n"))) (set! matrix_to_string_i (+ matrix_to_string_i 1)))) (throw (ex-info "return" {:v matrix_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_add [matrix_add_a matrix_add_b]
  (binding [matrix_add_i nil matrix_add_j nil matrix_add_res nil matrix_add_row nil] (try (do (when (or (not= (:rows matrix_add_a) (:rows matrix_add_b)) (not= (:cols matrix_add_a) (:cols matrix_add_b))) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! matrix_add_res []) (set! matrix_add_i 0) (while (< matrix_add_i (:rows matrix_add_a)) (do (set! matrix_add_row []) (set! matrix_add_j 0) (while (< matrix_add_j (:cols matrix_add_a)) (do (set! matrix_add_row (conj matrix_add_row (+ (get (get (:data matrix_add_a) matrix_add_i) matrix_add_j) (get (get (:data matrix_add_b) matrix_add_i) matrix_add_j)))) (set! matrix_add_j (+ matrix_add_j 1)))) (set! matrix_add_res (conj matrix_add_res matrix_add_row)) (set! matrix_add_i (+ matrix_add_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_add_a) :data matrix_add_res :rows (:rows matrix_add_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_sub [matrix_sub_a matrix_sub_b]
  (binding [matrix_sub_i nil matrix_sub_j nil matrix_sub_res nil matrix_sub_row nil] (try (do (when (or (not= (:rows matrix_sub_a) (:rows matrix_sub_b)) (not= (:cols matrix_sub_a) (:cols matrix_sub_b))) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! matrix_sub_res []) (set! matrix_sub_i 0) (while (< matrix_sub_i (:rows matrix_sub_a)) (do (set! matrix_sub_row []) (set! matrix_sub_j 0) (while (< matrix_sub_j (:cols matrix_sub_a)) (do (set! matrix_sub_row (conj matrix_sub_row (- (get (get (:data matrix_sub_a) matrix_sub_i) matrix_sub_j) (get (get (:data matrix_sub_b) matrix_sub_i) matrix_sub_j)))) (set! matrix_sub_j (+ matrix_sub_j 1)))) (set! matrix_sub_res (conj matrix_sub_res matrix_sub_row)) (set! matrix_sub_i (+ matrix_sub_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_sub_a) :data matrix_sub_res :rows (:rows matrix_sub_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul_scalar [matrix_mul_scalar_m matrix_mul_scalar_k]
  (binding [matrix_mul_scalar_i nil matrix_mul_scalar_j nil matrix_mul_scalar_res nil matrix_mul_scalar_row nil] (try (do (set! matrix_mul_scalar_res []) (set! matrix_mul_scalar_i 0) (while (< matrix_mul_scalar_i (:rows matrix_mul_scalar_m)) (do (set! matrix_mul_scalar_row []) (set! matrix_mul_scalar_j 0) (while (< matrix_mul_scalar_j (:cols matrix_mul_scalar_m)) (do (set! matrix_mul_scalar_row (conj matrix_mul_scalar_row (* (get (get (:data matrix_mul_scalar_m) matrix_mul_scalar_i) matrix_mul_scalar_j) matrix_mul_scalar_k))) (set! matrix_mul_scalar_j (+ matrix_mul_scalar_j 1)))) (set! matrix_mul_scalar_res (conj matrix_mul_scalar_res matrix_mul_scalar_row)) (set! matrix_mul_scalar_i (+ matrix_mul_scalar_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_mul_scalar_m) :data matrix_mul_scalar_res :rows (:rows matrix_mul_scalar_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_mul [matrix_mul_a matrix_mul_b]
  (binding [matrix_mul_i nil matrix_mul_j nil matrix_mul_k nil matrix_mul_res nil matrix_mul_row nil matrix_mul_sum nil] (try (do (when (not= (:cols matrix_mul_a) (:rows matrix_mul_b)) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! matrix_mul_res []) (set! matrix_mul_i 0) (while (< matrix_mul_i (:rows matrix_mul_a)) (do (set! matrix_mul_row []) (set! matrix_mul_j 0) (while (< matrix_mul_j (:cols matrix_mul_b)) (do (set! matrix_mul_sum 0.0) (set! matrix_mul_k 0) (while (< matrix_mul_k (:cols matrix_mul_a)) (do (set! matrix_mul_sum (+ matrix_mul_sum (* (get (get (:data matrix_mul_a) matrix_mul_i) matrix_mul_k) (get (get (:data matrix_mul_b) matrix_mul_k) matrix_mul_j)))) (set! matrix_mul_k (+ matrix_mul_k 1)))) (set! matrix_mul_row (conj matrix_mul_row matrix_mul_sum)) (set! matrix_mul_j (+ matrix_mul_j 1)))) (set! matrix_mul_res (conj matrix_mul_res matrix_mul_row)) (set! matrix_mul_i (+ matrix_mul_i 1)))) (throw (ex-info "return" {:v {:cols (:cols matrix_mul_b) :data matrix_mul_res :rows (:rows matrix_mul_a)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_transpose [matrix_transpose_m]
  (binding [matrix_transpose_c nil matrix_transpose_r nil matrix_transpose_res nil matrix_transpose_row nil] (try (do (set! matrix_transpose_res []) (set! matrix_transpose_c 0) (while (< matrix_transpose_c (:cols matrix_transpose_m)) (do (set! matrix_transpose_row []) (set! matrix_transpose_r 0) (while (< matrix_transpose_r (:rows matrix_transpose_m)) (do (set! matrix_transpose_row (conj matrix_transpose_row (get (get (:data matrix_transpose_m) matrix_transpose_r) matrix_transpose_c))) (set! matrix_transpose_r (+ matrix_transpose_r 1)))) (set! matrix_transpose_res (conj matrix_transpose_res matrix_transpose_row)) (set! matrix_transpose_c (+ matrix_transpose_c 1)))) (throw (ex-info "return" {:v {:cols (:rows matrix_transpose_m) :data matrix_transpose_res :rows (:cols matrix_transpose_m)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sherman_morrison [sherman_morrison_ainv sherman_morrison_u sherman_morrison_v]
  (binding [sherman_morrison_factor nil sherman_morrison_numerator nil sherman_morrison_scaled nil sherman_morrison_term1 nil sherman_morrison_term2 nil sherman_morrison_vt nil sherman_morrison_vu nil] (try (do (set! sherman_morrison_vt (matrix_transpose sherman_morrison_v)) (set! sherman_morrison_vu (matrix_mul (matrix_mul sherman_morrison_vt sherman_morrison_ainv) sherman_morrison_u)) (set! sherman_morrison_factor (+ (get (get (:data sherman_morrison_vu) 0) 0) 1.0)) (when (= sherman_morrison_factor 0.0) (throw (ex-info "return" {:v {:cols 0 :data [] :rows 0}}))) (set! sherman_morrison_term1 (matrix_mul sherman_morrison_ainv sherman_morrison_u)) (set! sherman_morrison_term2 (matrix_mul sherman_morrison_vt sherman_morrison_ainv)) (set! sherman_morrison_numerator (matrix_mul sherman_morrison_term1 sherman_morrison_term2)) (set! sherman_morrison_scaled (matrix_mul_scalar sherman_morrison_numerator (/ 1.0 sherman_morrison_factor))) (throw (ex-info "return" {:v (matrix_sub sherman_morrison_ainv sherman_morrison_scaled)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ainv nil main_result nil main_u nil main_v nil] (do (set! main_ainv (matrix_from_lists [[1.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0]])) (set! main_u (matrix_from_lists [[1.0] [2.0] [(- 3.0)]])) (set! main_v (matrix_from_lists [[4.0] [(- 2.0)] [5.0]])) (set! main_result (sherman_morrison main_ainv main_u main_v)) (println (matrix_to_string main_result)))))

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
