(ns main (:refer-clojure :exclude [subarray merge merge_sort]))

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

(declare subarray merge merge_sort)

(def ^:dynamic merge_i nil)

(def ^:dynamic merge_j nil)

(def ^:dynamic merge_result nil)

(def ^:dynamic merge_sort_left_half nil)

(def ^:dynamic merge_sort_middle nil)

(def ^:dynamic merge_sort_right_half nil)

(def ^:dynamic merge_sort_sorted_left nil)

(def ^:dynamic merge_sort_sorted_right nil)

(def ^:dynamic subarray_k nil)

(def ^:dynamic subarray_result nil)

(defn subarray [subarray_xs subarray_start subarray_end]
  (binding [subarray_k nil subarray_result nil] (try (do (set! subarray_result []) (set! subarray_k subarray_start) (while (< subarray_k subarray_end) (do (set! subarray_result (conj subarray_result (nth subarray_xs subarray_k))) (set! subarray_k (+ subarray_k 1)))) (throw (ex-info "return" {:v subarray_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge [merge_left_half merge_right_half]
  (binding [merge_i nil merge_j nil merge_result nil] (try (do (set! merge_result []) (set! merge_i 0) (set! merge_j 0) (while (and (< merge_i (count merge_left_half)) (< merge_j (count merge_right_half))) (if (< (nth merge_left_half merge_i) (nth merge_right_half merge_j)) (do (set! merge_result (conj merge_result (nth merge_left_half merge_i))) (set! merge_i (+ merge_i 1))) (do (set! merge_result (conj merge_result (nth merge_right_half merge_j))) (set! merge_j (+ merge_j 1))))) (while (< merge_i (count merge_left_half)) (do (set! merge_result (conj merge_result (nth merge_left_half merge_i))) (set! merge_i (+ merge_i 1)))) (while (< merge_j (count merge_right_half)) (do (set! merge_result (conj merge_result (nth merge_right_half merge_j))) (set! merge_j (+ merge_j 1)))) (throw (ex-info "return" {:v merge_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge_sort [merge_sort_array]
  (binding [merge_sort_left_half nil merge_sort_middle nil merge_sort_right_half nil merge_sort_sorted_left nil merge_sort_sorted_right nil] (try (do (when (<= (count merge_sort_array) 1) (throw (ex-info "return" {:v merge_sort_array}))) (set! merge_sort_middle (quot (count merge_sort_array) 2)) (set! merge_sort_left_half (subarray merge_sort_array 0 merge_sort_middle)) (set! merge_sort_right_half (subarray merge_sort_array merge_sort_middle (count merge_sort_array))) (set! merge_sort_sorted_left (merge_sort merge_sort_left_half)) (set! merge_sort_sorted_right (merge_sort merge_sort_right_half)) (throw (ex-info "return" {:v (merge merge_sort_sorted_left merge_sort_sorted_right)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (merge_sort [5 3 1 4 2])))
      (println (str (merge_sort [(- 2) 3 (- 10) 11 99 100000 100 (- 200)])))
      (println (str (merge_sort [(- 200)])))
      (println (str (merge_sort [])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
