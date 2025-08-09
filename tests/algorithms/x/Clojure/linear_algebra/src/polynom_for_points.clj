(ns main (:refer-clojure :exclude [contains_int split pow_int_float points_to_polynomial main]))

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

(declare contains_int split pow_int_float points_to_polynomial main)

(def ^:dynamic contains_int_i nil)

(def ^:dynamic count_v nil)

(def ^:dynamic points_to_polynomial_cc nil)

(def ^:dynamic points_to_polynomial_coeff nil)

(def ^:dynamic points_to_polynomial_col nil)

(def ^:dynamic points_to_polynomial_fraction nil)

(def ^:dynamic points_to_polynomial_i nil)

(def ^:dynamic points_to_polynomial_j nil)

(def ^:dynamic points_to_polynomial_k nil)

(def ^:dynamic points_to_polynomial_line nil)

(def ^:dynamic points_to_polynomial_matrix nil)

(def ^:dynamic points_to_polynomial_n nil)

(def ^:dynamic points_to_polynomial_number nil)

(def ^:dynamic points_to_polynomial_parts nil)

(def ^:dynamic points_to_polynomial_power nil)

(def ^:dynamic points_to_polynomial_row nil)

(def ^:dynamic points_to_polynomial_set_x nil)

(def ^:dynamic points_to_polynomial_solution nil)

(def ^:dynamic points_to_polynomial_solved nil)

(def ^:dynamic points_to_polynomial_value nil)

(def ^:dynamic points_to_polynomial_vector nil)

(def ^:dynamic points_to_polynomial_x_val nil)

(def ^:dynamic pow_int_float_i nil)

(def ^:dynamic pow_int_float_result nil)

(def ^:dynamic split_ch nil)

(def ^:dynamic split_current nil)

(def ^:dynamic split_i nil)

(def ^:dynamic split_res nil)

(defn contains_int [contains_int_xs contains_int_x]
  (binding [contains_int_i nil] (try (do (set! contains_int_i 0) (while (< contains_int_i (count contains_int_xs)) (do (when (= (nth contains_int_xs contains_int_i) contains_int_x) (throw (ex-info "return" {:v true}))) (set! contains_int_i (+ contains_int_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split [split_s split_sep]
  (binding [split_ch nil split_current nil split_i nil split_res nil] (try (do (set! split_res []) (set! split_current "") (set! split_i 0) (while (< split_i (count split_s)) (do (set! split_ch (subs split_s split_i (min (+ split_i 1) (count split_s)))) (if (= split_ch split_sep) (do (set! split_res (conj split_res split_current)) (set! split_current "")) (set! split_current (str split_current split_ch))) (set! split_i (+ split_i 1)))) (set! split_res (conj split_res split_current)) (throw (ex-info "return" {:v split_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_int_float [pow_int_float_base pow_int_float_exp]
  (binding [pow_int_float_i nil pow_int_float_result nil] (try (do (set! pow_int_float_result 1.0) (set! pow_int_float_i 0) (while (< pow_int_float_i pow_int_float_exp) (do (set! pow_int_float_result (* pow_int_float_result (double pow_int_float_base))) (set! pow_int_float_i (+ pow_int_float_i 1)))) (throw (ex-info "return" {:v pow_int_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn points_to_polynomial [points_to_polynomial_coordinates]
  (binding [count_v nil points_to_polynomial_cc nil points_to_polynomial_coeff nil points_to_polynomial_col nil points_to_polynomial_fraction nil points_to_polynomial_i nil points_to_polynomial_j nil points_to_polynomial_k nil points_to_polynomial_line nil points_to_polynomial_matrix nil points_to_polynomial_n nil points_to_polynomial_number nil points_to_polynomial_parts nil points_to_polynomial_power nil points_to_polynomial_row nil points_to_polynomial_set_x nil points_to_polynomial_solution nil points_to_polynomial_solved nil points_to_polynomial_value nil points_to_polynomial_vector nil points_to_polynomial_x_val nil] (try (do (when (= (count points_to_polynomial_coordinates) 0) (throw (Exception. "The program cannot work out a fitting polynomial."))) (set! points_to_polynomial_i 0) (while (< points_to_polynomial_i (count points_to_polynomial_coordinates)) (do (when (not= (count (nth points_to_polynomial_coordinates points_to_polynomial_i)) 2) (throw (Exception. "The program cannot work out a fitting polynomial."))) (set! points_to_polynomial_i (+ points_to_polynomial_i 1)))) (set! points_to_polynomial_j 0) (while (< points_to_polynomial_j (count points_to_polynomial_coordinates)) (do (set! points_to_polynomial_k (+ points_to_polynomial_j 1)) (while (< points_to_polynomial_k (count points_to_polynomial_coordinates)) (do (when (and (= (nth (nth points_to_polynomial_coordinates points_to_polynomial_j) 0) (nth (nth points_to_polynomial_coordinates points_to_polynomial_k) 0)) (= (nth (nth points_to_polynomial_coordinates points_to_polynomial_j) 1) (nth (nth points_to_polynomial_coordinates points_to_polynomial_k) 1))) (throw (Exception. "The program cannot work out a fitting polynomial."))) (set! points_to_polynomial_k (+ points_to_polynomial_k 1)))) (set! points_to_polynomial_j (+ points_to_polynomial_j 1)))) (set! points_to_polynomial_set_x []) (set! points_to_polynomial_i 0) (while (< points_to_polynomial_i (count points_to_polynomial_coordinates)) (do (set! points_to_polynomial_x_val (nth (nth points_to_polynomial_coordinates points_to_polynomial_i) 0)) (when (not (contains_int points_to_polynomial_set_x points_to_polynomial_x_val)) (set! points_to_polynomial_set_x (conj points_to_polynomial_set_x points_to_polynomial_x_val))) (set! points_to_polynomial_i (+ points_to_polynomial_i 1)))) (when (= (count points_to_polynomial_set_x) 1) (throw (ex-info "return" {:v (str "x=" (str (nth (nth points_to_polynomial_coordinates 0) 0)))}))) (when (not= (count points_to_polynomial_set_x) (count points_to_polynomial_coordinates)) (throw (Exception. "The program cannot work out a fitting polynomial."))) (set! points_to_polynomial_n (count points_to_polynomial_coordinates)) (set! points_to_polynomial_matrix []) (set! points_to_polynomial_row 0) (while (< points_to_polynomial_row points_to_polynomial_n) (do (set! points_to_polynomial_line []) (set! points_to_polynomial_col 0) (while (< points_to_polynomial_col points_to_polynomial_n) (do (set! points_to_polynomial_power (pow_int_float (nth (nth points_to_polynomial_coordinates points_to_polynomial_row) 0) (- points_to_polynomial_n (+ points_to_polynomial_col 1)))) (set! points_to_polynomial_line (conj points_to_polynomial_line points_to_polynomial_power)) (set! points_to_polynomial_col (+ points_to_polynomial_col 1)))) (set! points_to_polynomial_matrix (conj points_to_polynomial_matrix points_to_polynomial_line)) (set! points_to_polynomial_row (+ points_to_polynomial_row 1)))) (set! points_to_polynomial_vector []) (set! points_to_polynomial_row 0) (while (< points_to_polynomial_row points_to_polynomial_n) (do (set! points_to_polynomial_vector (conj points_to_polynomial_vector (double (nth (nth points_to_polynomial_coordinates points_to_polynomial_row) 1)))) (set! points_to_polynomial_row (+ points_to_polynomial_row 1)))) (set! count_v 0) (while (< count_v points_to_polynomial_n) (do (set! points_to_polynomial_number 0) (while (< points_to_polynomial_number points_to_polynomial_n) (do (when (not= count_v points_to_polynomial_number) (do (set! points_to_polynomial_fraction (quot (nth (nth points_to_polynomial_matrix points_to_polynomial_number) count_v) (nth (nth points_to_polynomial_matrix count_v) count_v))) (set! points_to_polynomial_cc 0) (while (< points_to_polynomial_cc points_to_polynomial_n) (do (set! points_to_polynomial_matrix (assoc-in points_to_polynomial_matrix [points_to_polynomial_number points_to_polynomial_cc] (- (nth (nth points_to_polynomial_matrix points_to_polynomial_number) points_to_polynomial_cc) (* (nth (nth points_to_polynomial_matrix count_v) points_to_polynomial_cc) points_to_polynomial_fraction)))) (set! points_to_polynomial_cc (+ points_to_polynomial_cc 1)))) (set! points_to_polynomial_vector (assoc points_to_polynomial_vector points_to_polynomial_number (- (nth points_to_polynomial_vector points_to_polynomial_number) (* (nth points_to_polynomial_vector count_v) points_to_polynomial_fraction)))))) (set! points_to_polynomial_number (+ points_to_polynomial_number 1)))) (set! count_v (+ count_v 1)))) (set! points_to_polynomial_solution []) (set! count_v 0) (while (< count_v points_to_polynomial_n) (do (set! points_to_polynomial_value (quot (nth points_to_polynomial_vector count_v) (nth (nth points_to_polynomial_matrix count_v) count_v))) (set! points_to_polynomial_solution (conj points_to_polynomial_solution (str points_to_polynomial_value))) (set! count_v (+ count_v 1)))) (set! points_to_polynomial_solved "f(x)=") (set! count_v 0) (while (< count_v points_to_polynomial_n) (do (set! points_to_polynomial_parts (split (nth points_to_polynomial_solution count_v) "e")) (set! points_to_polynomial_coeff (nth points_to_polynomial_solution count_v)) (when (> (count points_to_polynomial_parts) 1) (set! points_to_polynomial_coeff (str (str (nth points_to_polynomial_parts 0) "*10^") (nth points_to_polynomial_parts 1)))) (set! points_to_polynomial_solved (str (str (str (str points_to_polynomial_solved "x^") (str (- points_to_polynomial_n (+ count_v 1)))) "*") points_to_polynomial_coeff)) (when (not= (+ count_v 1) points_to_polynomial_n) (set! points_to_polynomial_solved (str points_to_polynomial_solved "+"))) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v points_to_polynomial_solved}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (points_to_polynomial [[1 0] [2 0] [3 0]])) (println (points_to_polynomial [[1 1] [2 1] [3 1]])) (println (points_to_polynomial [[1 1] [2 4] [3 9]])) (println (points_to_polynomial [[1 3] [2 6] [3 11]])) (println (points_to_polynomial [[1 (- 3)] [2 (- 6)] [3 (- 11)]])) (println (points_to_polynomial [[1 1] [1 2] [1 3]])) (println (points_to_polynomial [[1 5] [2 2] [3 9]]))))

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
