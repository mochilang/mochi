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

(defn toi [s]
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare subarray merge merge_sort)

(def ^:dynamic merge_i nil)

(def ^:dynamic merge_j nil)

(def ^:dynamic merge_result nil)

(def ^:dynamic merge_sort_left nil)

(def ^:dynamic merge_sort_mid_index nil)

(def ^:dynamic merge_sort_right nil)

(def ^:dynamic merge_sort_sorted_left nil)

(def ^:dynamic merge_sort_sorted_right nil)

(def ^:dynamic subarray_i nil)

(def ^:dynamic subarray_result nil)

(defn subarray [subarray_xs subarray_start subarray_end]
  (binding [subarray_i nil subarray_result nil] (try (do (set! subarray_result []) (set! subarray_i subarray_start) (while (< subarray_i subarray_end) (do (set! subarray_result (conj subarray_result (nth subarray_xs subarray_i))) (set! subarray_i (+ subarray_i 1)))) (throw (ex-info "return" {:v subarray_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge [merge_left merge_right]
  (binding [merge_i nil merge_j nil merge_result nil] (try (do (set! merge_result []) (set! merge_i 0) (set! merge_j 0) (while (and (< merge_i (count merge_left)) (< merge_j (count merge_right))) (if (<= (nth merge_left merge_i) (nth merge_right merge_j)) (do (set! merge_result (conj merge_result (nth merge_left merge_i))) (set! merge_i (+ merge_i 1))) (do (set! merge_result (conj merge_result (nth merge_right merge_j))) (set! merge_j (+ merge_j 1))))) (while (< merge_i (count merge_left)) (do (set! merge_result (conj merge_result (nth merge_left merge_i))) (set! merge_i (+ merge_i 1)))) (while (< merge_j (count merge_right)) (do (set! merge_result (conj merge_result (nth merge_right merge_j))) (set! merge_j (+ merge_j 1)))) (throw (ex-info "return" {:v merge_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge_sort [merge_sort_collection]
  (binding [merge_sort_left nil merge_sort_mid_index nil merge_sort_right nil merge_sort_sorted_left nil merge_sort_sorted_right nil] (try (do (when (<= (count merge_sort_collection) 1) (throw (ex-info "return" {:v merge_sort_collection}))) (set! merge_sort_mid_index (/ (count merge_sort_collection) 2)) (set! merge_sort_left (subarray merge_sort_collection 0 merge_sort_mid_index)) (set! merge_sort_right (subarray merge_sort_collection merge_sort_mid_index (count merge_sort_collection))) (set! merge_sort_sorted_left (merge_sort merge_sort_left)) (set! merge_sort_sorted_right (merge_sort merge_sort_right)) (throw (ex-info "return" {:v (merge merge_sort_sorted_left merge_sort_sorted_right)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (merge_sort [0 5 3 2 2])))
      (println (str (merge_sort [])))
      (println (str (merge_sort [(- 2) (- 5) (- 45)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
