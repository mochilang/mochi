(ns main (:refer-clojure :exclude [heapify build_max_heap top_k_frequent_words main]))

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

(declare heapify build_max_heap top_k_frequent_words main)

(def ^:dynamic build_max_heap_i nil)

(def ^:dynamic heapify_arr nil)

(def ^:dynamic heapify_largest nil)

(def ^:dynamic heapify_largest_item nil)

(def ^:dynamic heapify_largest_item2 nil)

(def ^:dynamic heapify_left nil)

(def ^:dynamic heapify_left_item nil)

(def ^:dynamic heapify_right nil)

(def ^:dynamic heapify_right_item nil)

(def ^:dynamic heapify_temp nil)

(def ^:dynamic main_sample nil)

(def ^:dynamic top_k_frequent_words_heap nil)

(def ^:dynamic top_k_frequent_words_heap_size nil)

(def ^:dynamic top_k_frequent_words_i nil)

(def ^:dynamic top_k_frequent_words_item nil)

(def ^:dynamic top_k_frequent_words_j nil)

(def ^:dynamic top_k_frequent_words_limit nil)

(def ^:dynamic top_k_frequent_words_result nil)

(def ^:dynamic top_k_frequent_words_w nil)

(def ^:dynamic main_freq_map {})

(defn heapify [heapify_arr_p heapify_index heapify_heap_size]
  (binding [heapify_arr nil heapify_largest nil heapify_largest_item nil heapify_largest_item2 nil heapify_left nil heapify_left_item nil heapify_right nil heapify_right_item nil heapify_temp nil] (do (set! heapify_arr heapify_arr_p) (set! heapify_largest heapify_index) (set! heapify_left (+ (* 2 heapify_index) 1)) (set! heapify_right (+ (* 2 heapify_index) 2)) (when (< heapify_left heapify_heap_size) (do (set! heapify_left_item (nth heapify_arr heapify_left)) (set! heapify_largest_item (nth heapify_arr heapify_largest)) (when (> (:count heapify_left_item) (:count heapify_largest_item)) (set! heapify_largest heapify_left)))) (when (< heapify_right heapify_heap_size) (do (set! heapify_right_item (nth heapify_arr heapify_right)) (set! heapify_largest_item2 (nth heapify_arr heapify_largest)) (when (> (:count heapify_right_item) (:count heapify_largest_item2)) (set! heapify_largest heapify_right)))) (when (not= heapify_largest heapify_index) (do (set! heapify_temp (nth heapify_arr heapify_largest)) (set! heapify_arr (assoc heapify_arr heapify_largest (nth heapify_arr heapify_index))) (set! heapify_arr (assoc heapify_arr heapify_index heapify_temp)) (heapify heapify_arr heapify_largest heapify_heap_size))) heapify_arr)))

(defn build_max_heap [build_max_heap_arr]
  (binding [build_max_heap_i nil] (do (set! build_max_heap_i (- (/ (count build_max_heap_arr) 2) 1)) (while (>= build_max_heap_i 0) (do (heapify build_max_heap_arr build_max_heap_i (count build_max_heap_arr)) (set! build_max_heap_i (- build_max_heap_i 1)))) build_max_heap_arr)))

(defn top_k_frequent_words [top_k_frequent_words_words top_k_frequent_words_k_value]
  (binding [top_k_frequent_words_heap nil top_k_frequent_words_heap_size nil top_k_frequent_words_i nil top_k_frequent_words_item nil top_k_frequent_words_j nil top_k_frequent_words_limit nil top_k_frequent_words_result nil top_k_frequent_words_w nil] (try (do (alter-var-root (var main_freq_map) (fn [_] {})) (set! top_k_frequent_words_i 0) (while (< top_k_frequent_words_i (count top_k_frequent_words_words)) (do (set! top_k_frequent_words_w (nth top_k_frequent_words_words top_k_frequent_words_i)) (if (in top_k_frequent_words_w main_freq_map) (alter-var-root (var main_freq_map) (fn [_] (assoc main_freq_map top_k_frequent_words_w (+ (get main_freq_map top_k_frequent_words_w) 1)))) (alter-var-root (var main_freq_map) (fn [_] (assoc main_freq_map top_k_frequent_words_w 1)))) (set! top_k_frequent_words_i (+ top_k_frequent_words_i 1)))) (set! top_k_frequent_words_heap []) (doseq [w (keys main_freq_map)] (set! top_k_frequent_words_heap (conj top_k_frequent_words_heap {:count (get main_freq_map top_k_frequent_words_w) :word top_k_frequent_words_w}))) (build_max_heap top_k_frequent_words_heap) (set! top_k_frequent_words_result []) (set! top_k_frequent_words_heap_size (count top_k_frequent_words_heap)) (set! top_k_frequent_words_limit top_k_frequent_words_k_value) (when (> top_k_frequent_words_limit top_k_frequent_words_heap_size) (set! top_k_frequent_words_limit top_k_frequent_words_heap_size)) (set! top_k_frequent_words_j 0) (while (< top_k_frequent_words_j top_k_frequent_words_limit) (do (set! top_k_frequent_words_item (nth top_k_frequent_words_heap 0)) (set! top_k_frequent_words_result (conj top_k_frequent_words_result (:word top_k_frequent_words_item))) (set! top_k_frequent_words_heap (assoc top_k_frequent_words_heap 0 (nth top_k_frequent_words_heap (- top_k_frequent_words_heap_size 1)))) (set! top_k_frequent_words_heap (assoc top_k_frequent_words_heap (- top_k_frequent_words_heap_size 1) top_k_frequent_words_item)) (set! top_k_frequent_words_heap_size (- top_k_frequent_words_heap_size 1)) (heapify top_k_frequent_words_heap 0 top_k_frequent_words_heap_size) (set! top_k_frequent_words_j (+ top_k_frequent_words_j 1)))) (throw (ex-info "return" {:v top_k_frequent_words_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_sample nil] (do (set! main_sample ["a" "b" "c" "a" "c" "c"]) (println (top_k_frequent_words main_sample 3)) (println (top_k_frequent_words main_sample 2)) (println (top_k_frequent_words main_sample 1)) (println (top_k_frequent_words main_sample 0)))))

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
