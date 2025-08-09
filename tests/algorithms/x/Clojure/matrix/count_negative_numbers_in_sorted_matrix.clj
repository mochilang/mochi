(ns main (:refer-clojure :exclude [generate_large_matrix find_negative_index count_negatives_binary_search count_negatives_brute_force count_negatives_brute_force_with_break]))

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

(declare generate_large_matrix find_negative_index count_negatives_binary_search count_negatives_brute_force count_negatives_brute_force_with_break)

(def ^:dynamic count_negatives_binary_search_bound nil)

(def ^:dynamic count_negatives_binary_search_i nil)

(def ^:dynamic count_negatives_binary_search_idx nil)

(def ^:dynamic count_negatives_binary_search_row nil)

(def ^:dynamic count_negatives_binary_search_total nil)

(def ^:dynamic count_negatives_brute_force_i nil)

(def ^:dynamic count_negatives_brute_force_j nil)

(def ^:dynamic count_negatives_brute_force_row nil)

(def ^:dynamic count_negatives_brute_force_with_break_i nil)

(def ^:dynamic count_negatives_brute_force_with_break_j nil)

(def ^:dynamic count_negatives_brute_force_with_break_number nil)

(def ^:dynamic count_negatives_brute_force_with_break_row nil)

(def ^:dynamic count_negatives_brute_force_with_break_total nil)

(def ^:dynamic count_v nil)

(def ^:dynamic find_negative_index_left nil)

(def ^:dynamic find_negative_index_mid nil)

(def ^:dynamic find_negative_index_num nil)

(def ^:dynamic find_negative_index_right nil)

(def ^:dynamic generate_large_matrix_i nil)

(def ^:dynamic generate_large_matrix_j nil)

(def ^:dynamic generate_large_matrix_result nil)

(def ^:dynamic generate_large_matrix_row nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_results_bin nil)

(def ^:dynamic main_results_break nil)

(def ^:dynamic main_results_brute nil)

(defn generate_large_matrix []
  (binding [generate_large_matrix_i nil generate_large_matrix_j nil generate_large_matrix_result nil generate_large_matrix_row nil] (try (do (set! generate_large_matrix_result []) (set! generate_large_matrix_i 0) (while (< generate_large_matrix_i 1000) (do (set! generate_large_matrix_row []) (set! generate_large_matrix_j (- 1000 generate_large_matrix_i)) (while (> generate_large_matrix_j (- (- 1000) generate_large_matrix_i)) (do (set! generate_large_matrix_row (conj generate_large_matrix_row generate_large_matrix_j)) (set! generate_large_matrix_j (- generate_large_matrix_j 1)))) (set! generate_large_matrix_result (conj generate_large_matrix_result generate_large_matrix_row)) (set! generate_large_matrix_i (+ generate_large_matrix_i 1)))) (throw (ex-info "return" {:v generate_large_matrix_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_negative_index [find_negative_index_arr]
  (binding [find_negative_index_left nil find_negative_index_mid nil find_negative_index_num nil find_negative_index_right nil] (try (do (set! find_negative_index_left 0) (set! find_negative_index_right (- (count find_negative_index_arr) 1)) (when (= (count find_negative_index_arr) 0) (throw (ex-info "return" {:v 0}))) (when (< (nth find_negative_index_arr 0) 0) (throw (ex-info "return" {:v 0}))) (while (<= find_negative_index_left find_negative_index_right) (do (set! find_negative_index_mid (quot (+ find_negative_index_left find_negative_index_right) 2)) (set! find_negative_index_num (nth find_negative_index_arr find_negative_index_mid)) (if (< find_negative_index_num 0) (do (when (= find_negative_index_mid 0) (throw (ex-info "return" {:v 0}))) (when (>= (nth find_negative_index_arr (- find_negative_index_mid 1)) 0) (throw (ex-info "return" {:v find_negative_index_mid}))) (set! find_negative_index_right (- find_negative_index_mid 1))) (set! find_negative_index_left (+ find_negative_index_mid 1))))) (throw (ex-info "return" {:v (count find_negative_index_arr)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_negatives_binary_search [count_negatives_binary_search_grid]
  (binding [count_negatives_binary_search_bound nil count_negatives_binary_search_i nil count_negatives_binary_search_idx nil count_negatives_binary_search_row nil count_negatives_binary_search_total nil] (try (do (set! count_negatives_binary_search_total 0) (set! count_negatives_binary_search_bound (count (nth count_negatives_binary_search_grid 0))) (set! count_negatives_binary_search_i 0) (while (< count_negatives_binary_search_i (count count_negatives_binary_search_grid)) (do (set! count_negatives_binary_search_row (nth count_negatives_binary_search_grid count_negatives_binary_search_i)) (set! count_negatives_binary_search_idx (find_negative_index (subvec count_negatives_binary_search_row 0 (min count_negatives_binary_search_bound (count count_negatives_binary_search_row))))) (set! count_negatives_binary_search_bound count_negatives_binary_search_idx) (set! count_negatives_binary_search_total (+ count_negatives_binary_search_total count_negatives_binary_search_idx)) (set! count_negatives_binary_search_i (+ count_negatives_binary_search_i 1)))) (throw (ex-info "return" {:v (- (* (count count_negatives_binary_search_grid) (count (nth count_negatives_binary_search_grid 0))) count_negatives_binary_search_total)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_negatives_brute_force [count_negatives_brute_force_grid]
  (binding [count_negatives_brute_force_i nil count_negatives_brute_force_j nil count_negatives_brute_force_row nil count_v nil] (try (do (set! count_v 0) (set! count_negatives_brute_force_i 0) (while (< count_negatives_brute_force_i (count count_negatives_brute_force_grid)) (do (set! count_negatives_brute_force_row (nth count_negatives_brute_force_grid count_negatives_brute_force_i)) (set! count_negatives_brute_force_j 0) (while (< count_negatives_brute_force_j (count count_negatives_brute_force_row)) (do (when (< (nth count_negatives_brute_force_row count_negatives_brute_force_j) 0) (set! count_v (+ count_v 1))) (set! count_negatives_brute_force_j (+ count_negatives_brute_force_j 1)))) (set! count_negatives_brute_force_i (+ count_negatives_brute_force_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_negatives_brute_force_with_break [count_negatives_brute_force_with_break_grid]
  (binding [count_negatives_brute_force_with_break_i nil count_negatives_brute_force_with_break_j nil count_negatives_brute_force_with_break_number nil count_negatives_brute_force_with_break_row nil count_negatives_brute_force_with_break_total nil] (try (do (set! count_negatives_brute_force_with_break_total 0) (set! count_negatives_brute_force_with_break_i 0) (while (< count_negatives_brute_force_with_break_i (count count_negatives_brute_force_with_break_grid)) (do (set! count_negatives_brute_force_with_break_row (nth count_negatives_brute_force_with_break_grid count_negatives_brute_force_with_break_i)) (set! count_negatives_brute_force_with_break_j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< count_negatives_brute_force_with_break_j (count count_negatives_brute_force_with_break_row))) (do (set! count_negatives_brute_force_with_break_number (nth count_negatives_brute_force_with_break_row count_negatives_brute_force_with_break_j)) (cond (< count_negatives_brute_force_with_break_number 0) (do (set! count_negatives_brute_force_with_break_total (+ count_negatives_brute_force_with_break_total (- (count count_negatives_brute_force_with_break_row) count_negatives_brute_force_with_break_j))) (recur false)) :else (do (set! count_negatives_brute_force_with_break_j (+ count_negatives_brute_force_with_break_j 1)) (recur while_flag_1)))))) (set! count_negatives_brute_force_with_break_i (+ count_negatives_brute_force_with_break_i 1)))) (throw (ex-info "return" {:v count_negatives_brute_force_with_break_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_grid (generate_large_matrix))

(def ^:dynamic main_test_grids [[[4 3 2 (- 1)] [3 2 1 (- 1)] [1 1 (- 1) (- 2)] [(- 1) (- 1) (- 2) (- 3)]] [[3 2] [1 0]] [[7 7 6]] [[7 7 6] [(- 1) (- 2) (- 3)]] main_grid])

(def ^:dynamic main_results_bin [])

(def ^:dynamic main_i 0)

(def ^:dynamic main_results_brute [])

(def ^:dynamic main_results_break [])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i (count main_test_grids)) (do (def main_results_bin (conj main_results_bin (count_negatives_binary_search (nth main_test_grids main_i)))) (def main_i (+ main_i 1))))
      (println (str main_results_bin))
      (def main_i 0)
      (while (< main_i (count main_test_grids)) (do (def main_results_brute (conj main_results_brute (count_negatives_brute_force (nth main_test_grids main_i)))) (def main_i (+ main_i 1))))
      (println (str main_results_brute))
      (def main_i 0)
      (while (< main_i (count main_test_grids)) (do (def main_results_break (conj main_results_break (count_negatives_brute_force_with_break (nth main_test_grids main_i)))) (def main_i (+ main_i 1))))
      (println (str main_results_break))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
