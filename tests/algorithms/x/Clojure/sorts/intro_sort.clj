(ns main (:refer-clojure :exclude [insertion_sort heapify heap_sort median_of_3 partition int_log2 intro_sort intro_sort_main]))

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

(declare insertion_sort heapify heap_sort median_of_3 partition int_log2 intro_sort intro_sort_main)

(def ^:dynamic heap_sort_arr nil)

(def ^:dynamic heap_sort_i nil)

(def ^:dynamic heap_sort_n nil)

(def ^:dynamic heap_sort_temp nil)

(def ^:dynamic heapify_arr nil)

(def ^:dynamic heapify_largest nil)

(def ^:dynamic heapify_left nil)

(def ^:dynamic heapify_right nil)

(def ^:dynamic heapify_temp nil)

(def ^:dynamic insertion_sort_arr nil)

(def ^:dynamic insertion_sort_i nil)

(def ^:dynamic insertion_sort_j nil)

(def ^:dynamic insertion_sort_key nil)

(def ^:dynamic int_log2_r nil)

(def ^:dynamic int_log2_v nil)

(def ^:dynamic intro_sort__ nil)

(def ^:dynamic intro_sort_array nil)

(def ^:dynamic intro_sort_depth nil)

(def ^:dynamic intro_sort_e nil)

(def ^:dynamic intro_sort_main_max_depth nil)

(def ^:dynamic intro_sort_main_sorted nil)

(def ^:dynamic intro_sort_p nil)

(def ^:dynamic intro_sort_pivot nil)

(def ^:dynamic intro_sort_res nil)

(def ^:dynamic intro_sort_s nil)

(def ^:dynamic median_of_3_a nil)

(def ^:dynamic median_of_3_b nil)

(def ^:dynamic median_of_3_c nil)

(def ^:dynamic partition_arr nil)

(def ^:dynamic partition_i nil)

(def ^:dynamic partition_j nil)

(def ^:dynamic partition_temp nil)

(defn insertion_sort [insertion_sort_a insertion_sort_start insertion_sort_end_]
  (binding [insertion_sort_arr nil insertion_sort_i nil insertion_sort_j nil insertion_sort_key nil] (try (do (set! insertion_sort_arr insertion_sort_a) (set! insertion_sort_i insertion_sort_start) (while (< insertion_sort_i insertion_sort_end_) (do (set! insertion_sort_key (nth insertion_sort_arr insertion_sort_i)) (set! insertion_sort_j insertion_sort_i) (while (and (> insertion_sort_j insertion_sort_start) (> (nth insertion_sort_arr (- insertion_sort_j 1)) insertion_sort_key)) (do (set! insertion_sort_arr (assoc insertion_sort_arr insertion_sort_j (nth insertion_sort_arr (- insertion_sort_j 1)))) (set! insertion_sort_j (- insertion_sort_j 1)))) (set! insertion_sort_arr (assoc insertion_sort_arr insertion_sort_j insertion_sort_key)) (set! insertion_sort_i (+ insertion_sort_i 1)))) (throw (ex-info "return" {:v insertion_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn heapify [heapify_a heapify_index heapify_heap_size]
  (binding [heapify_arr nil heapify_largest nil heapify_left nil heapify_right nil heapify_temp nil] (try (do (set! heapify_arr heapify_a) (set! heapify_largest heapify_index) (set! heapify_left (+ (* 2 heapify_index) 1)) (set! heapify_right (+ (* 2 heapify_index) 2)) (when (and (< heapify_left heapify_heap_size) (> (nth heapify_arr heapify_left) (nth heapify_arr heapify_largest))) (set! heapify_largest heapify_left)) (when (and (< heapify_right heapify_heap_size) (> (nth heapify_arr heapify_right) (nth heapify_arr heapify_largest))) (set! heapify_largest heapify_right)) (when (not= heapify_largest heapify_index) (do (set! heapify_temp (nth heapify_arr heapify_index)) (set! heapify_arr (assoc heapify_arr heapify_index (nth heapify_arr heapify_largest))) (set! heapify_arr (assoc heapify_arr heapify_largest heapify_temp)) (set! heapify_arr (heapify heapify_arr heapify_largest heapify_heap_size)))) (throw (ex-info "return" {:v heapify_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn heap_sort [heap_sort_a]
  (binding [heap_sort_arr nil heap_sort_i nil heap_sort_n nil heap_sort_temp nil] (try (do (set! heap_sort_arr heap_sort_a) (set! heap_sort_n (count heap_sort_arr)) (when (<= heap_sort_n 1) (throw (ex-info "return" {:v heap_sort_arr}))) (set! heap_sort_i (/ heap_sort_n 2)) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! heap_sort_arr (heapify heap_sort_arr heap_sort_i heap_sort_n)) (cond (= heap_sort_i 0) (recur false) :else (do (set! heap_sort_i (- heap_sort_i 1)) (recur while_flag_1)))))) (set! heap_sort_i (- heap_sort_n 1)) (while (> heap_sort_i 0) (do (set! heap_sort_temp (nth heap_sort_arr 0)) (set! heap_sort_arr (assoc heap_sort_arr 0 (nth heap_sort_arr heap_sort_i))) (set! heap_sort_arr (assoc heap_sort_arr heap_sort_i heap_sort_temp)) (set! heap_sort_arr (heapify heap_sort_arr 0 heap_sort_i)) (set! heap_sort_i (- heap_sort_i 1)))) (throw (ex-info "return" {:v heap_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn median_of_3 [median_of_3_arr first_v median_of_3_middle median_of_3_last]
  (binding [median_of_3_a nil median_of_3_b nil median_of_3_c nil] (try (do (set! median_of_3_a (nth median_of_3_arr first_v)) (set! median_of_3_b (nth median_of_3_arr median_of_3_middle)) (set! median_of_3_c (nth median_of_3_arr median_of_3_last)) (if (or (and (> median_of_3_a median_of_3_b) (< median_of_3_a median_of_3_c)) (and (< median_of_3_a median_of_3_b) (> median_of_3_a median_of_3_c))) (throw (ex-info "return" {:v median_of_3_a})) (if (or (and (> median_of_3_b median_of_3_a) (< median_of_3_b median_of_3_c)) (and (< median_of_3_b median_of_3_a) (> median_of_3_b median_of_3_c))) (throw (ex-info "return" {:v median_of_3_b})) (throw (ex-info "return" {:v median_of_3_c}))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn partition [partition_arr_p partition_low partition_high partition_pivot]
  (binding [partition_arr nil partition_i nil partition_j nil partition_temp nil] (try (do (set! partition_arr partition_arr_p) (set! partition_i partition_low) (set! partition_j partition_high) (while true (do (while (< (nth partition_arr partition_i) partition_pivot) (set! partition_i (+ partition_i 1))) (set! partition_j (- partition_j 1)) (while (< partition_pivot (nth partition_arr partition_j)) (set! partition_j (- partition_j 1))) (when (>= partition_i partition_j) (throw (ex-info "return" {:v partition_i}))) (set! partition_temp (nth partition_arr partition_i)) (set! partition_arr (assoc partition_arr partition_i (nth partition_arr partition_j))) (set! partition_arr (assoc partition_arr partition_j partition_temp)) (set! partition_i (+ partition_i 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn int_log2 [int_log2_n]
  (binding [int_log2_r nil int_log2_v nil] (try (do (set! int_log2_v int_log2_n) (set! int_log2_r 0) (while (> int_log2_v 1) (do (set! int_log2_v (/ int_log2_v 2)) (set! int_log2_r (+ int_log2_r 1)))) (throw (ex-info "return" {:v int_log2_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn intro_sort [intro_sort_arr intro_sort_start intro_sort_end_ intro_sort_size_threshold intro_sort_max_depth]
  (binding [intro_sort__ nil intro_sort_array nil intro_sort_depth nil intro_sort_e nil intro_sort_p nil intro_sort_pivot nil intro_sort_res nil intro_sort_s nil] (try (do (set! intro_sort_array intro_sort_arr) (set! intro_sort_s intro_sort_start) (set! intro_sort_e intro_sort_end_) (set! intro_sort_depth intro_sort_max_depth) (while (> (- intro_sort_e intro_sort_s) intro_sort_size_threshold) (do (when (= intro_sort_depth 0) (throw (ex-info "return" {:v (heap_sort intro_sort_array)}))) (set! intro_sort_depth (- intro_sort_depth 1)) (set! intro_sort_pivot (median_of_3 intro_sort_array intro_sort_s (+ (+ intro_sort_s (/ (- intro_sort_e intro_sort_s) 2)) 1) (- intro_sort_e 1))) (set! intro_sort_p (partition intro_sort_array intro_sort_s intro_sort_e intro_sort_pivot)) (set! intro_sort_array (intro_sort intro_sort_array intro_sort_p intro_sort_e intro_sort_size_threshold intro_sort_depth)) (set! intro_sort_e intro_sort_p))) (set! intro_sort_res (insertion_sort intro_sort_array intro_sort_s intro_sort_e)) (set! intro_sort__ (count intro_sort_res)) (throw (ex-info "return" {:v intro_sort_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn intro_sort_main [intro_sort_main_arr]
  (binding [intro_sort_main_max_depth nil intro_sort_main_sorted nil] (try (do (when (= (count intro_sort_main_arr) 0) (do (println intro_sort_main_arr) (throw (ex-info "return" {:v nil})))) (set! intro_sort_main_max_depth (* 2 (int_log2 (count intro_sort_main_arr)))) (set! intro_sort_main_sorted (intro_sort intro_sort_main_arr 0 (count intro_sort_main_arr) 16 intro_sort_main_max_depth)) (println intro_sort_main_sorted)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example1 [4 2 6 8 1 7 8 22 14 56 27 79 23 45 14 12])

(def ^:dynamic main_example2 [21 15 11 45 (- 2) (- 11) 46])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (intro_sort_main main_example1)
      (intro_sort_main main_example2)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
