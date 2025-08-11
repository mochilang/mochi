(ns main (:refer-clojure :exclude [binary_search_insertion_from binary_search_insertion merge sortlist_2d merge_insertion_sort main]))

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

(declare binary_search_insertion_from binary_search_insertion merge sortlist_2d merge_insertion_sort main)

(def ^:dynamic binary_search_insertion_from_i nil)

(def ^:dynamic binary_search_insertion_from_left nil)

(def ^:dynamic binary_search_insertion_from_middle nil)

(def ^:dynamic binary_search_insertion_from_result nil)

(def ^:dynamic binary_search_insertion_from_right nil)

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic main_example3 nil)

(def ^:dynamic merge_i nil)

(def ^:dynamic merge_insertion_sort_a nil)

(def ^:dynamic merge_insertion_sort_b nil)

(def ^:dynamic merge_insertion_sort_has_last_odd_item nil)

(def ^:dynamic merge_insertion_sort_i nil)

(def ^:dynamic merge_insertion_sort_idx nil)

(def ^:dynamic merge_insertion_sort_inserted_before nil)

(def ^:dynamic merge_insertion_sort_pivot nil)

(def ^:dynamic merge_insertion_sort_result nil)

(def ^:dynamic merge_insertion_sort_sorted_list_2d nil)

(def ^:dynamic merge_insertion_sort_two_paired_list nil)

(def ^:dynamic merge_j nil)

(def ^:dynamic merge_result nil)

(def ^:dynamic sortlist_2d_i nil)

(def ^:dynamic sortlist_2d_j nil)

(def ^:dynamic sortlist_2d_left nil)

(def ^:dynamic sortlist_2d_length nil)

(def ^:dynamic sortlist_2d_middle nil)

(def ^:dynamic sortlist_2d_right nil)

(defn binary_search_insertion_from [binary_search_insertion_from_sorted_list binary_search_insertion_from_item binary_search_insertion_from_start]
  (binding [binary_search_insertion_from_i nil binary_search_insertion_from_left nil binary_search_insertion_from_middle nil binary_search_insertion_from_result nil binary_search_insertion_from_right nil] (try (do (set! binary_search_insertion_from_left binary_search_insertion_from_start) (set! binary_search_insertion_from_right (- (count binary_search_insertion_from_sorted_list) 1)) (loop [while_flag_1 true] (when (and while_flag_1 (<= binary_search_insertion_from_left binary_search_insertion_from_right)) (do (set! binary_search_insertion_from_middle (/ (+ binary_search_insertion_from_left binary_search_insertion_from_right) 2)) (if (= binary_search_insertion_from_left binary_search_insertion_from_right) (do (when (< (nth binary_search_insertion_from_sorted_list binary_search_insertion_from_middle) binary_search_insertion_from_item) (set! binary_search_insertion_from_left (+ binary_search_insertion_from_middle 1))) (recur false)) (if (< (nth binary_search_insertion_from_sorted_list binary_search_insertion_from_middle) binary_search_insertion_from_item) (set! binary_search_insertion_from_left (+ binary_search_insertion_from_middle 1)) (set! binary_search_insertion_from_right (- binary_search_insertion_from_middle 1)))) (cond :else (recur while_flag_1))))) (set! binary_search_insertion_from_result []) (set! binary_search_insertion_from_i 0) (while (< binary_search_insertion_from_i binary_search_insertion_from_left) (do (set! binary_search_insertion_from_result (conj binary_search_insertion_from_result (nth binary_search_insertion_from_sorted_list binary_search_insertion_from_i))) (set! binary_search_insertion_from_i (+ binary_search_insertion_from_i 1)))) (set! binary_search_insertion_from_result (conj binary_search_insertion_from_result binary_search_insertion_from_item)) (while (< binary_search_insertion_from_i (count binary_search_insertion_from_sorted_list)) (do (set! binary_search_insertion_from_result (conj binary_search_insertion_from_result (nth binary_search_insertion_from_sorted_list binary_search_insertion_from_i))) (set! binary_search_insertion_from_i (+ binary_search_insertion_from_i 1)))) (throw (ex-info "return" {:v binary_search_insertion_from_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_search_insertion [binary_search_insertion_sorted_list binary_search_insertion_item]
  (try (throw (ex-info "return" {:v (binary_search_insertion_from binary_search_insertion_sorted_list binary_search_insertion_item 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn merge [merge_left merge_right]
  (binding [merge_i nil merge_j nil merge_result nil] (try (do (set! merge_result []) (set! merge_i 0) (set! merge_j 0) (while (and (< merge_i (count merge_left)) (< merge_j (count merge_right))) (if (< (nth (nth merge_left merge_i) 0) (nth (nth merge_right merge_j) 0)) (do (set! merge_result (conj merge_result (nth merge_left merge_i))) (set! merge_i (+ merge_i 1))) (do (set! merge_result (conj merge_result (nth merge_right merge_j))) (set! merge_j (+ merge_j 1))))) (while (< merge_i (count merge_left)) (do (set! merge_result (conj merge_result (nth merge_left merge_i))) (set! merge_i (+ merge_i 1)))) (while (< merge_j (count merge_right)) (do (set! merge_result (conj merge_result (nth merge_right merge_j))) (set! merge_j (+ merge_j 1)))) (throw (ex-info "return" {:v merge_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sortlist_2d [sortlist_2d_list_2d]
  (binding [sortlist_2d_i nil sortlist_2d_j nil sortlist_2d_left nil sortlist_2d_length nil sortlist_2d_middle nil sortlist_2d_right nil] (try (do (set! sortlist_2d_length (count sortlist_2d_list_2d)) (when (<= sortlist_2d_length 1) (throw (ex-info "return" {:v sortlist_2d_list_2d}))) (set! sortlist_2d_middle (/ sortlist_2d_length 2)) (set! sortlist_2d_left []) (set! sortlist_2d_i 0) (while (< sortlist_2d_i sortlist_2d_middle) (do (set! sortlist_2d_left (conj sortlist_2d_left (nth sortlist_2d_list_2d sortlist_2d_i))) (set! sortlist_2d_i (+ sortlist_2d_i 1)))) (set! sortlist_2d_right []) (set! sortlist_2d_j sortlist_2d_middle) (while (< sortlist_2d_j sortlist_2d_length) (do (set! sortlist_2d_right (conj sortlist_2d_right (nth sortlist_2d_list_2d sortlist_2d_j))) (set! sortlist_2d_j (+ sortlist_2d_j 1)))) (throw (ex-info "return" {:v (merge (sortlist_2d sortlist_2d_left) (sortlist_2d sortlist_2d_right))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge_insertion_sort [merge_insertion_sort_collection]
  (binding [merge_insertion_sort_a nil merge_insertion_sort_b nil merge_insertion_sort_has_last_odd_item nil merge_insertion_sort_i nil merge_insertion_sort_idx nil merge_insertion_sort_inserted_before nil merge_insertion_sort_pivot nil merge_insertion_sort_result nil merge_insertion_sort_sorted_list_2d nil merge_insertion_sort_two_paired_list nil] (try (do (when (<= (count merge_insertion_sort_collection) 1) (throw (ex-info "return" {:v merge_insertion_sort_collection}))) (set! merge_insertion_sort_two_paired_list []) (set! merge_insertion_sort_has_last_odd_item false) (set! merge_insertion_sort_i 0) (while (< merge_insertion_sort_i (count merge_insertion_sort_collection)) (do (if (= merge_insertion_sort_i (- (count merge_insertion_sort_collection) 1)) (set! merge_insertion_sort_has_last_odd_item true) (do (set! merge_insertion_sort_a (nth merge_insertion_sort_collection merge_insertion_sort_i)) (set! merge_insertion_sort_b (nth merge_insertion_sort_collection (+ merge_insertion_sort_i 1))) (if (< merge_insertion_sort_a merge_insertion_sort_b) (set! merge_insertion_sort_two_paired_list (conj merge_insertion_sort_two_paired_list [merge_insertion_sort_a merge_insertion_sort_b])) (set! merge_insertion_sort_two_paired_list (conj merge_insertion_sort_two_paired_list [merge_insertion_sort_b merge_insertion_sort_a]))))) (set! merge_insertion_sort_i (+ merge_insertion_sort_i 2)))) (set! merge_insertion_sort_sorted_list_2d (sortlist_2d merge_insertion_sort_two_paired_list)) (set! merge_insertion_sort_result []) (set! merge_insertion_sort_i 0) (while (< merge_insertion_sort_i (count merge_insertion_sort_sorted_list_2d)) (do (set! merge_insertion_sort_result (conj merge_insertion_sort_result (nth (nth merge_insertion_sort_sorted_list_2d merge_insertion_sort_i) 0))) (set! merge_insertion_sort_i (+ merge_insertion_sort_i 1)))) (set! merge_insertion_sort_result (conj merge_insertion_sort_result (nth (nth merge_insertion_sort_sorted_list_2d (- (count merge_insertion_sort_sorted_list_2d) 1)) 1))) (when merge_insertion_sort_has_last_odd_item (set! merge_insertion_sort_result (binary_search_insertion merge_insertion_sort_result (nth merge_insertion_sort_collection (- (count merge_insertion_sort_collection) 1))))) (set! merge_insertion_sort_inserted_before false) (set! merge_insertion_sort_idx 0) (while (< merge_insertion_sort_idx (- (count merge_insertion_sort_sorted_list_2d) 1)) (do (when (and merge_insertion_sort_has_last_odd_item (= (nth merge_insertion_sort_result merge_insertion_sort_idx) (nth merge_insertion_sort_collection (- (count merge_insertion_sort_collection) 1)))) (set! merge_insertion_sort_inserted_before true)) (set! merge_insertion_sort_pivot (nth (nth merge_insertion_sort_sorted_list_2d merge_insertion_sort_idx) 1)) (if merge_insertion_sort_inserted_before (set! merge_insertion_sort_result (binary_search_insertion_from merge_insertion_sort_result merge_insertion_sort_pivot (+ merge_insertion_sort_idx 2))) (set! merge_insertion_sort_result (binary_search_insertion_from merge_insertion_sort_result merge_insertion_sort_pivot (+ merge_insertion_sort_idx 1)))) (set! merge_insertion_sort_idx (+ merge_insertion_sort_idx 1)))) (throw (ex-info "return" {:v merge_insertion_sort_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_example1 nil main_example2 nil main_example3 nil] (do (set! main_example1 [0 5 3 2 2]) (set! main_example2 [99]) (set! main_example3 [(- 2) (- 5) (- 45)]) (println (str (merge_insertion_sort main_example1))) (println (str (merge_insertion_sort main_example2))) (println (str (merge_insertion_sort main_example3))))))

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
