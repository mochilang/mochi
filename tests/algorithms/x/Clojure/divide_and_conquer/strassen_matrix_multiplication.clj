(ns main (:refer-clojure :exclude [default_matrix_multiplication matrix_addition matrix_subtraction split_matrix matrix_dimensions next_power_of_two pad_matrix actual_strassen strassen main]))

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

(declare default_matrix_multiplication matrix_addition matrix_subtraction split_matrix matrix_dimensions next_power_of_two pad_matrix actual_strassen strassen main)

(def ^:dynamic actual_strassen_a nil)

(def ^:dynamic actual_strassen_b nil)

(def ^:dynamic actual_strassen_bot_left nil)

(def ^:dynamic actual_strassen_bot_right nil)

(def ^:dynamic actual_strassen_c nil)

(def ^:dynamic actual_strassen_d nil)

(def ^:dynamic actual_strassen_e nil)

(def ^:dynamic actual_strassen_f nil)

(def ^:dynamic actual_strassen_g nil)

(def ^:dynamic actual_strassen_h nil)

(def ^:dynamic actual_strassen_i nil)

(def ^:dynamic actual_strassen_new_matrix nil)

(def ^:dynamic actual_strassen_parts_a nil)

(def ^:dynamic actual_strassen_parts_b nil)

(def ^:dynamic actual_strassen_t1 nil)

(def ^:dynamic actual_strassen_t2 nil)

(def ^:dynamic actual_strassen_t3 nil)

(def ^:dynamic actual_strassen_t4 nil)

(def ^:dynamic actual_strassen_t5 nil)

(def ^:dynamic actual_strassen_t6 nil)

(def ^:dynamic actual_strassen_t7 nil)

(def ^:dynamic actual_strassen_top_left nil)

(def ^:dynamic actual_strassen_top_right nil)

(def ^:dynamic main_matrix1 nil)

(def ^:dynamic main_matrix2 nil)

(def ^:dynamic main_res nil)

(def ^:dynamic matrix_addition_i nil)

(def ^:dynamic matrix_addition_j nil)

(def ^:dynamic matrix_addition_result nil)

(def ^:dynamic matrix_addition_row nil)

(def ^:dynamic matrix_subtraction_i nil)

(def ^:dynamic matrix_subtraction_j nil)

(def ^:dynamic matrix_subtraction_result nil)

(def ^:dynamic matrix_subtraction_row nil)

(def ^:dynamic next_power_of_two_p nil)

(def ^:dynamic pad_matrix_i nil)

(def ^:dynamic pad_matrix_j nil)

(def ^:dynamic pad_matrix_res nil)

(def ^:dynamic pad_matrix_row nil)

(def ^:dynamic pad_matrix_v nil)

(def ^:dynamic split_matrix_bot_left nil)

(def ^:dynamic split_matrix_bot_right nil)

(def ^:dynamic split_matrix_i nil)

(def ^:dynamic split_matrix_j nil)

(def ^:dynamic split_matrix_left_row nil)

(def ^:dynamic split_matrix_mid nil)

(def ^:dynamic split_matrix_n nil)

(def ^:dynamic split_matrix_right_row nil)

(def ^:dynamic split_matrix_top_left nil)

(def ^:dynamic split_matrix_top_right nil)

(def ^:dynamic strassen_dims1 nil)

(def ^:dynamic strassen_dims2 nil)

(def ^:dynamic strassen_final_matrix nil)

(def ^:dynamic strassen_i nil)

(def ^:dynamic strassen_j nil)

(def ^:dynamic strassen_maximum nil)

(def ^:dynamic strassen_new_matrix1 nil)

(def ^:dynamic strassen_new_matrix2 nil)

(def ^:dynamic strassen_result_padded nil)

(def ^:dynamic strassen_row nil)

(def ^:dynamic strassen_size nil)

(defn default_matrix_multiplication [default_matrix_multiplication_a default_matrix_multiplication_b]
  (try (throw (ex-info "return" {:v [[(+ (* (nth (nth default_matrix_multiplication_a 0) 0) (nth (nth default_matrix_multiplication_b 0) 0)) (* (nth (nth default_matrix_multiplication_a 0) 1) (nth (nth default_matrix_multiplication_b 1) 0))) (+ (* (nth (nth default_matrix_multiplication_a 0) 0) (nth (nth default_matrix_multiplication_b 0) 1)) (* (nth (nth default_matrix_multiplication_a 0) 1) (nth (nth default_matrix_multiplication_b 1) 1)))] [(+ (* (nth (nth default_matrix_multiplication_a 1) 0) (nth (nth default_matrix_multiplication_b 0) 0)) (* (nth (nth default_matrix_multiplication_a 1) 1) (nth (nth default_matrix_multiplication_b 1) 0))) (+ (* (nth (nth default_matrix_multiplication_a 1) 0) (nth (nth default_matrix_multiplication_b 0) 1)) (* (nth (nth default_matrix_multiplication_a 1) 1) (nth (nth default_matrix_multiplication_b 1) 1)))]]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn matrix_addition [matrix_addition_matrix_a matrix_addition_matrix_b]
  (binding [matrix_addition_i nil matrix_addition_j nil matrix_addition_result nil matrix_addition_row nil] (try (do (set! matrix_addition_result []) (set! matrix_addition_i 0) (while (< matrix_addition_i (count matrix_addition_matrix_a)) (do (set! matrix_addition_row []) (set! matrix_addition_j 0) (while (< matrix_addition_j (count (nth matrix_addition_matrix_a matrix_addition_i))) (do (set! matrix_addition_row (conj matrix_addition_row (+ (nth (nth matrix_addition_matrix_a matrix_addition_i) matrix_addition_j) (nth (nth matrix_addition_matrix_b matrix_addition_i) matrix_addition_j)))) (set! matrix_addition_j (+ matrix_addition_j 1)))) (set! matrix_addition_result (conj matrix_addition_result matrix_addition_row)) (set! matrix_addition_i (+ matrix_addition_i 1)))) (throw (ex-info "return" {:v matrix_addition_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_subtraction [matrix_subtraction_matrix_a matrix_subtraction_matrix_b]
  (binding [matrix_subtraction_i nil matrix_subtraction_j nil matrix_subtraction_result nil matrix_subtraction_row nil] (try (do (set! matrix_subtraction_result []) (set! matrix_subtraction_i 0) (while (< matrix_subtraction_i (count matrix_subtraction_matrix_a)) (do (set! matrix_subtraction_row []) (set! matrix_subtraction_j 0) (while (< matrix_subtraction_j (count (nth matrix_subtraction_matrix_a matrix_subtraction_i))) (do (set! matrix_subtraction_row (conj matrix_subtraction_row (- (nth (nth matrix_subtraction_matrix_a matrix_subtraction_i) matrix_subtraction_j) (nth (nth matrix_subtraction_matrix_b matrix_subtraction_i) matrix_subtraction_j)))) (set! matrix_subtraction_j (+ matrix_subtraction_j 1)))) (set! matrix_subtraction_result (conj matrix_subtraction_result matrix_subtraction_row)) (set! matrix_subtraction_i (+ matrix_subtraction_i 1)))) (throw (ex-info "return" {:v matrix_subtraction_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split_matrix [split_matrix_a]
  (binding [split_matrix_bot_left nil split_matrix_bot_right nil split_matrix_i nil split_matrix_j nil split_matrix_left_row nil split_matrix_mid nil split_matrix_n nil split_matrix_right_row nil split_matrix_top_left nil split_matrix_top_right nil] (try (do (set! split_matrix_n (count split_matrix_a)) (set! split_matrix_mid (quot split_matrix_n 2)) (set! split_matrix_top_left []) (set! split_matrix_top_right []) (set! split_matrix_bot_left []) (set! split_matrix_bot_right []) (set! split_matrix_i 0) (while (< split_matrix_i split_matrix_mid) (do (set! split_matrix_left_row []) (set! split_matrix_right_row []) (set! split_matrix_j 0) (while (< split_matrix_j split_matrix_mid) (do (set! split_matrix_left_row (conj split_matrix_left_row (nth (nth split_matrix_a split_matrix_i) split_matrix_j))) (set! split_matrix_right_row (conj split_matrix_right_row (nth (nth split_matrix_a split_matrix_i) (+ split_matrix_j split_matrix_mid)))) (set! split_matrix_j (+ split_matrix_j 1)))) (set! split_matrix_top_left (conj split_matrix_top_left split_matrix_left_row)) (set! split_matrix_top_right (conj split_matrix_top_right split_matrix_right_row)) (set! split_matrix_i (+ split_matrix_i 1)))) (set! split_matrix_i split_matrix_mid) (while (< split_matrix_i split_matrix_n) (do (set! split_matrix_left_row []) (set! split_matrix_right_row []) (set! split_matrix_j 0) (while (< split_matrix_j split_matrix_mid) (do (set! split_matrix_left_row (conj split_matrix_left_row (nth (nth split_matrix_a split_matrix_i) split_matrix_j))) (set! split_matrix_right_row (conj split_matrix_right_row (nth (nth split_matrix_a split_matrix_i) (+ split_matrix_j split_matrix_mid)))) (set! split_matrix_j (+ split_matrix_j 1)))) (set! split_matrix_bot_left (conj split_matrix_bot_left split_matrix_left_row)) (set! split_matrix_bot_right (conj split_matrix_bot_right split_matrix_right_row)) (set! split_matrix_i (+ split_matrix_i 1)))) (throw (ex-info "return" {:v [split_matrix_top_left split_matrix_top_right split_matrix_bot_left split_matrix_bot_right]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_dimensions [matrix_dimensions_matrix]
  (try (throw (ex-info "return" {:v [(count matrix_dimensions_matrix) (count (nth matrix_dimensions_matrix 0))]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn next_power_of_two [next_power_of_two_n]
  (binding [next_power_of_two_p nil] (try (do (set! next_power_of_two_p 1) (while (< next_power_of_two_p next_power_of_two_n) (set! next_power_of_two_p (* next_power_of_two_p 2))) (throw (ex-info "return" {:v next_power_of_two_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pad_matrix [pad_matrix_mat pad_matrix_rows pad_matrix_cols]
  (binding [pad_matrix_i nil pad_matrix_j nil pad_matrix_res nil pad_matrix_row nil pad_matrix_v nil] (try (do (set! pad_matrix_res []) (set! pad_matrix_i 0) (while (< pad_matrix_i pad_matrix_rows) (do (set! pad_matrix_row []) (set! pad_matrix_j 0) (while (< pad_matrix_j pad_matrix_cols) (do (set! pad_matrix_v 0) (when (and (< pad_matrix_i (count pad_matrix_mat)) (< pad_matrix_j (count (nth pad_matrix_mat 0)))) (set! pad_matrix_v (nth (nth pad_matrix_mat pad_matrix_i) pad_matrix_j))) (set! pad_matrix_row (conj pad_matrix_row pad_matrix_v)) (set! pad_matrix_j (+ pad_matrix_j 1)))) (set! pad_matrix_res (conj pad_matrix_res pad_matrix_row)) (set! pad_matrix_i (+ pad_matrix_i 1)))) (throw (ex-info "return" {:v pad_matrix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn actual_strassen [actual_strassen_matrix_a actual_strassen_matrix_b]
  (binding [actual_strassen_a nil actual_strassen_b nil actual_strassen_bot_left nil actual_strassen_bot_right nil actual_strassen_c nil actual_strassen_d nil actual_strassen_e nil actual_strassen_f nil actual_strassen_g nil actual_strassen_h nil actual_strassen_i nil actual_strassen_new_matrix nil actual_strassen_parts_a nil actual_strassen_parts_b nil actual_strassen_t1 nil actual_strassen_t2 nil actual_strassen_t3 nil actual_strassen_t4 nil actual_strassen_t5 nil actual_strassen_t6 nil actual_strassen_t7 nil actual_strassen_top_left nil actual_strassen_top_right nil] (try (do (when (= (nth (matrix_dimensions actual_strassen_matrix_a) 0) 2) (throw (ex-info "return" {:v (default_matrix_multiplication actual_strassen_matrix_a actual_strassen_matrix_b)}))) (set! actual_strassen_parts_a (split_matrix actual_strassen_matrix_a)) (set! actual_strassen_a (nth actual_strassen_parts_a 0)) (set! actual_strassen_b (nth actual_strassen_parts_a 1)) (set! actual_strassen_c (nth actual_strassen_parts_a 2)) (set! actual_strassen_d (nth actual_strassen_parts_a 3)) (set! actual_strassen_parts_b (split_matrix actual_strassen_matrix_b)) (set! actual_strassen_e (nth actual_strassen_parts_b 0)) (set! actual_strassen_f (nth actual_strassen_parts_b 1)) (set! actual_strassen_g (nth actual_strassen_parts_b 2)) (set! actual_strassen_h (nth actual_strassen_parts_b 3)) (set! actual_strassen_t1 (actual_strassen actual_strassen_a (matrix_subtraction actual_strassen_f actual_strassen_h))) (set! actual_strassen_t2 (actual_strassen (matrix_addition actual_strassen_a actual_strassen_b) actual_strassen_h)) (set! actual_strassen_t3 (actual_strassen (matrix_addition actual_strassen_c actual_strassen_d) actual_strassen_e)) (set! actual_strassen_t4 (actual_strassen actual_strassen_d (matrix_subtraction actual_strassen_g actual_strassen_e))) (set! actual_strassen_t5 (actual_strassen (matrix_addition actual_strassen_a actual_strassen_d) (matrix_addition actual_strassen_e actual_strassen_h))) (set! actual_strassen_t6 (actual_strassen (matrix_subtraction actual_strassen_b actual_strassen_d) (matrix_addition actual_strassen_g actual_strassen_h))) (set! actual_strassen_t7 (actual_strassen (matrix_subtraction actual_strassen_a actual_strassen_c) (matrix_addition actual_strassen_e actual_strassen_f))) (set! actual_strassen_top_left (matrix_addition (matrix_subtraction (matrix_addition actual_strassen_t5 actual_strassen_t4) actual_strassen_t2) actual_strassen_t6)) (set! actual_strassen_top_right (matrix_addition actual_strassen_t1 actual_strassen_t2)) (set! actual_strassen_bot_left (matrix_addition actual_strassen_t3 actual_strassen_t4)) (set! actual_strassen_bot_right (matrix_subtraction (matrix_subtraction (matrix_addition actual_strassen_t1 actual_strassen_t5) actual_strassen_t3) actual_strassen_t7)) (set! actual_strassen_new_matrix []) (set! actual_strassen_i 0) (while (< actual_strassen_i (count actual_strassen_top_right)) (do (set! actual_strassen_new_matrix (conj actual_strassen_new_matrix (concat (nth actual_strassen_top_left actual_strassen_i) (nth actual_strassen_top_right actual_strassen_i)))) (set! actual_strassen_i (+ actual_strassen_i 1)))) (set! actual_strassen_i 0) (while (< actual_strassen_i (count actual_strassen_bot_right)) (do (set! actual_strassen_new_matrix (conj actual_strassen_new_matrix (concat (nth actual_strassen_bot_left actual_strassen_i) (nth actual_strassen_bot_right actual_strassen_i)))) (set! actual_strassen_i (+ actual_strassen_i 1)))) (throw (ex-info "return" {:v actual_strassen_new_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn strassen [strassen_matrix1 strassen_matrix2]
  (binding [strassen_dims1 nil strassen_dims2 nil strassen_final_matrix nil strassen_i nil strassen_j nil strassen_maximum nil strassen_new_matrix1 nil strassen_new_matrix2 nil strassen_result_padded nil strassen_row nil strassen_size nil] (try (do (set! strassen_dims1 (matrix_dimensions strassen_matrix1)) (set! strassen_dims2 (matrix_dimensions strassen_matrix2)) (when (not= (nth strassen_dims1 1) (nth strassen_dims2 0)) (throw (ex-info "return" {:v []}))) (set! strassen_maximum (long (apply max [(nth strassen_dims1 0) (nth strassen_dims1 1) (nth strassen_dims2 0) (nth strassen_dims2 1)]))) (set! strassen_size (next_power_of_two strassen_maximum)) (set! strassen_new_matrix1 (pad_matrix strassen_matrix1 strassen_size strassen_size)) (set! strassen_new_matrix2 (pad_matrix strassen_matrix2 strassen_size strassen_size)) (set! strassen_result_padded (actual_strassen strassen_new_matrix1 strassen_new_matrix2)) (set! strassen_final_matrix []) (set! strassen_i 0) (while (< strassen_i (nth strassen_dims1 0)) (do (set! strassen_row []) (set! strassen_j 0) (while (< strassen_j (nth strassen_dims2 1)) (do (set! strassen_row (conj strassen_row (nth (nth strassen_result_padded strassen_i) strassen_j))) (set! strassen_j (+ strassen_j 1)))) (set! strassen_final_matrix (conj strassen_final_matrix strassen_row)) (set! strassen_i (+ strassen_i 1)))) (throw (ex-info "return" {:v strassen_final_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_matrix1 nil main_matrix2 nil main_res nil] (do (set! main_matrix1 [[2 3 4 5] [6 4 3 1] [2 3 6 7] [3 1 2 4] [2 3 4 5] [6 4 3 1] [2 3 6 7] [3 1 2 4] [2 3 4 5] [6 2 3 1]]) (set! main_matrix2 [[0 2 1 1] [16 2 3 3] [2 2 7 7] [13 11 22 4]]) (set! main_res (strassen main_matrix1 main_matrix2)) (println main_res))))

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
