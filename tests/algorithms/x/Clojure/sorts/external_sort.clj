(ns main (:refer-clojure :exclude [subarray merge merge_sort split_into_blocks merge_blocks external_sort main]))

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

(declare subarray merge merge_sort split_into_blocks merge_blocks external_sort main)

(def ^:dynamic external_sort_blocks nil)

(def ^:dynamic main_data nil)

(def ^:dynamic main_sorted_data nil)

(def ^:dynamic merge_blocks_done nil)

(def ^:dynamic merge_blocks_i nil)

(def ^:dynamic merge_blocks_idx nil)

(def ^:dynamic merge_blocks_indices nil)

(def ^:dynamic merge_blocks_j nil)

(def ^:dynamic merge_blocks_min_block nil)

(def ^:dynamic merge_blocks_min_val nil)

(def ^:dynamic merge_blocks_num_blocks nil)

(def ^:dynamic merge_blocks_result nil)

(def ^:dynamic merge_blocks_val nil)

(def ^:dynamic merge_i nil)

(def ^:dynamic merge_j nil)

(def ^:dynamic merge_result nil)

(def ^:dynamic merge_sort_left_half nil)

(def ^:dynamic merge_sort_middle nil)

(def ^:dynamic merge_sort_right_half nil)

(def ^:dynamic merge_sort_sorted_left nil)

(def ^:dynamic merge_sort_sorted_right nil)

(def ^:dynamic split_into_blocks_block nil)

(def ^:dynamic split_into_blocks_blocks nil)

(def ^:dynamic split_into_blocks_end nil)

(def ^:dynamic split_into_blocks_i nil)

(def ^:dynamic split_into_blocks_sorted_block nil)

(def ^:dynamic subarray_k nil)

(def ^:dynamic subarray_result nil)

(defn subarray [subarray_xs subarray_start subarray_end]
  (binding [subarray_k nil subarray_result nil] (try (do (set! subarray_result []) (set! subarray_k subarray_start) (while (< subarray_k subarray_end) (do (set! subarray_result (conj subarray_result (nth subarray_xs subarray_k))) (set! subarray_k (+ subarray_k 1)))) (throw (ex-info "return" {:v subarray_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge [merge_left_half merge_right_half]
  (binding [merge_i nil merge_j nil merge_result nil] (try (do (set! merge_result []) (set! merge_i 0) (set! merge_j 0) (while (and (< merge_i (count merge_left_half)) (< merge_j (count merge_right_half))) (if (< (nth merge_left_half merge_i) (nth merge_right_half merge_j)) (do (set! merge_result (conj merge_result (nth merge_left_half merge_i))) (set! merge_i (+ merge_i 1))) (do (set! merge_result (conj merge_result (nth merge_right_half merge_j))) (set! merge_j (+ merge_j 1))))) (while (< merge_i (count merge_left_half)) (do (set! merge_result (conj merge_result (nth merge_left_half merge_i))) (set! merge_i (+ merge_i 1)))) (while (< merge_j (count merge_right_half)) (do (set! merge_result (conj merge_result (nth merge_right_half merge_j))) (set! merge_j (+ merge_j 1)))) (throw (ex-info "return" {:v merge_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge_sort [merge_sort_array]
  (binding [merge_sort_left_half nil merge_sort_middle nil merge_sort_right_half nil merge_sort_sorted_left nil merge_sort_sorted_right nil] (try (do (when (<= (count merge_sort_array) 1) (throw (ex-info "return" {:v merge_sort_array}))) (set! merge_sort_middle (/ (count merge_sort_array) 2)) (set! merge_sort_left_half (subarray merge_sort_array 0 merge_sort_middle)) (set! merge_sort_right_half (subarray merge_sort_array merge_sort_middle (count merge_sort_array))) (set! merge_sort_sorted_left (merge_sort merge_sort_left_half)) (set! merge_sort_sorted_right (merge_sort merge_sort_right_half)) (throw (ex-info "return" {:v (merge merge_sort_sorted_left merge_sort_sorted_right)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split_into_blocks [split_into_blocks_data split_into_blocks_block_size]
  (binding [split_into_blocks_block nil split_into_blocks_blocks nil split_into_blocks_end nil split_into_blocks_i nil split_into_blocks_sorted_block nil] (try (do (set! split_into_blocks_blocks []) (set! split_into_blocks_i 0) (while (< split_into_blocks_i (count split_into_blocks_data)) (do (set! split_into_blocks_end (if (< (+ split_into_blocks_i split_into_blocks_block_size) (count split_into_blocks_data)) (+ split_into_blocks_i split_into_blocks_block_size) (count split_into_blocks_data))) (set! split_into_blocks_block (subarray split_into_blocks_data split_into_blocks_i split_into_blocks_end)) (set! split_into_blocks_sorted_block (merge_sort split_into_blocks_block)) (set! split_into_blocks_blocks (conj split_into_blocks_blocks split_into_blocks_sorted_block)) (set! split_into_blocks_i split_into_blocks_end))) (throw (ex-info "return" {:v split_into_blocks_blocks}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge_blocks [merge_blocks_blocks]
  (binding [merge_blocks_done nil merge_blocks_i nil merge_blocks_idx nil merge_blocks_indices nil merge_blocks_j nil merge_blocks_min_block nil merge_blocks_min_val nil merge_blocks_num_blocks nil merge_blocks_result nil merge_blocks_val nil] (try (do (set! merge_blocks_num_blocks (count merge_blocks_blocks)) (set! merge_blocks_indices []) (set! merge_blocks_i 0) (while (< merge_blocks_i merge_blocks_num_blocks) (do (set! merge_blocks_indices (conj merge_blocks_indices 0)) (set! merge_blocks_i (+ merge_blocks_i 1)))) (set! merge_blocks_result []) (set! merge_blocks_done false) (while (not merge_blocks_done) (do (set! merge_blocks_done true) (set! merge_blocks_min_val 0) (set! merge_blocks_min_block (- 0 1)) (set! merge_blocks_j 0) (while (< merge_blocks_j merge_blocks_num_blocks) (do (set! merge_blocks_idx (nth merge_blocks_indices merge_blocks_j)) (when (< merge_blocks_idx (count (nth merge_blocks_blocks merge_blocks_j))) (do (set! merge_blocks_val (nth (nth merge_blocks_blocks merge_blocks_j) merge_blocks_idx)) (when (or (= merge_blocks_min_block (- 0 1)) (< merge_blocks_val merge_blocks_min_val)) (do (set! merge_blocks_min_val merge_blocks_val) (set! merge_blocks_min_block merge_blocks_j))) (set! merge_blocks_done false))) (set! merge_blocks_j (+ merge_blocks_j 1)))) (when (not merge_blocks_done) (do (set! merge_blocks_result (conj merge_blocks_result merge_blocks_min_val)) (set! merge_blocks_indices (assoc merge_blocks_indices merge_blocks_min_block (+ (nth merge_blocks_indices merge_blocks_min_block) 1))))))) (throw (ex-info "return" {:v merge_blocks_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn external_sort [external_sort_data external_sort_block_size]
  (binding [external_sort_blocks nil] (try (do (set! external_sort_blocks (split_into_blocks external_sort_data external_sort_block_size)) (throw (ex-info "return" {:v (merge_blocks external_sort_blocks)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_data nil main_sorted_data nil] (do (set! main_data [7 1 5 3 9 2 6 4 8 0]) (set! main_sorted_data (external_sort main_data 3)) (println main_sorted_data))))

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
