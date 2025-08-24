(ns main (:refer-clojure :exclude [parent_index left_child_idx right_child_idx max_heapify build_max_heap extract_max insert heap_sort heap_to_string]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parent_index left_child_idx right_child_idx max_heapify build_max_heap extract_max insert heap_sort heap_to_string)

(declare _read_file)

(def ^:dynamic build_max_heap_heap_size nil)

(def ^:dynamic build_max_heap_i nil)

(def ^:dynamic extract_max_h nil)

(def ^:dynamic extract_max_max_value nil)

(def ^:dynamic heap_sort_h nil)

(def ^:dynamic heap_sort_j nil)

(def ^:dynamic heap_sort_size nil)

(def ^:dynamic heap_sort_temp nil)

(def ^:dynamic heap_to_string_i nil)

(def ^:dynamic heap_to_string_s nil)

(def ^:dynamic insert_h nil)

(def ^:dynamic insert_heap_size nil)

(def ^:dynamic insert_idx nil)

(def ^:dynamic main_size nil)

(def ^:dynamic max_heapify_h nil)

(def ^:dynamic max_heapify_largest nil)

(def ^:dynamic max_heapify_left nil)

(def ^:dynamic max_heapify_right nil)

(def ^:dynamic max_heapify_temp nil)

(defn parent_index [parent_index_child_idx]
  (try (if (> parent_index_child_idx 0) (quot (- parent_index_child_idx 1) 2) (- 1)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn left_child_idx [left_child_idx_parent_idx]
  (try (throw (ex-info "return" {:v (+ (* 2 left_child_idx_parent_idx) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn right_child_idx [right_child_idx_parent_idx]
  (try (throw (ex-info "return" {:v (+ (* 2 right_child_idx_parent_idx) 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn max_heapify [max_heapify_h_p max_heapify_heap_size max_heapify_index]
  (binding [max_heapify_h max_heapify_h_p max_heapify_largest nil max_heapify_left nil max_heapify_right nil max_heapify_temp nil] (do (try (do (set! max_heapify_largest max_heapify_index) (set! max_heapify_left (left_child_idx max_heapify_index)) (set! max_heapify_right (right_child_idx max_heapify_index)) (when (and (< max_heapify_left max_heapify_heap_size) (> (nth max_heapify_h max_heapify_left) (nth max_heapify_h max_heapify_largest))) (set! max_heapify_largest max_heapify_left)) (when (and (< max_heapify_right max_heapify_heap_size) (> (nth max_heapify_h max_heapify_right) (nth max_heapify_h max_heapify_largest))) (set! max_heapify_largest max_heapify_right)) (when (not= max_heapify_largest max_heapify_index) (do (set! max_heapify_temp (nth max_heapify_h max_heapify_index)) (set! max_heapify_h (assoc max_heapify_h max_heapify_index (nth max_heapify_h max_heapify_largest))) (set! max_heapify_h (assoc max_heapify_h max_heapify_largest max_heapify_temp)) (let [__res (max_heapify max_heapify_h max_heapify_heap_size max_heapify_largest)] (do (set! max_heapify_h max_heapify_h) __res))))) (finally (alter-var-root (var max_heapify_h) (constantly max_heapify_h)))) max_heapify_h)))

(defn build_max_heap [build_max_heap_h]
  (binding [build_max_heap_heap_size nil build_max_heap_i nil] (try (do (set! build_max_heap_heap_size (count build_max_heap_h)) (set! build_max_heap_i (- (quot build_max_heap_heap_size 2) 1)) (while (>= build_max_heap_i 0) (do (let [__res (max_heapify build_max_heap_h build_max_heap_heap_size build_max_heap_i)] (do (set! build_max_heap_h max_heapify_h) __res)) (set! build_max_heap_i (- build_max_heap_i 1)))) (throw (ex-info "return" {:v build_max_heap_heap_size}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extract_max [extract_max_h_p extract_max_heap_size]
  (binding [extract_max_h extract_max_h_p extract_max_max_value nil] (try (do (set! extract_max_max_value (nth extract_max_h 0)) (set! extract_max_h (assoc extract_max_h 0 (nth extract_max_h (- extract_max_heap_size 1)))) (let [__res (max_heapify extract_max_h (- extract_max_heap_size 1) 0)] (do (set! extract_max_h max_heapify_h) __res)) (throw (ex-info "return" {:v extract_max_max_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var extract_max_h) (constantly extract_max_h))))))

(defn insert [insert_h_p insert_heap_size_p insert_value]
  (binding [insert_h insert_h_p insert_heap_size insert_heap_size_p insert_idx nil] (try (do (if (< insert_heap_size (count insert_h)) (set! insert_h (assoc insert_h insert_heap_size insert_value)) (set! insert_h (conj insert_h insert_value))) (set! insert_heap_size (+ insert_heap_size 1)) (set! insert_idx (quot (- insert_heap_size 1) 2)) (while (>= insert_idx 0) (do (let [__res (max_heapify insert_h insert_heap_size insert_idx)] (do (set! insert_h max_heapify_h) __res)) (set! insert_idx (quot (- insert_idx 1) 2)))) (throw (ex-info "return" {:v insert_heap_size}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var insert_h) (constantly insert_h)) (alter-var-root (var insert_heap_size) (constantly insert_heap_size))))))

(defn heap_sort [heap_sort_h_p heap_sort_heap_size]
  (binding [heap_sort_h heap_sort_h_p heap_sort_j nil heap_sort_size nil heap_sort_temp nil] (do (try (do (set! heap_sort_size heap_sort_heap_size) (set! heap_sort_j (- heap_sort_size 1)) (while (> heap_sort_j 0) (do (set! heap_sort_temp (nth heap_sort_h 0)) (set! heap_sort_h (assoc heap_sort_h 0 (nth heap_sort_h heap_sort_j))) (set! heap_sort_h (assoc heap_sort_h heap_sort_j heap_sort_temp)) (set! heap_sort_size (- heap_sort_size 1)) (let [__res (max_heapify heap_sort_h heap_sort_size 0)] (do (set! heap_sort_h max_heapify_h) __res)) (set! heap_sort_j (- heap_sort_j 1))))) (finally (alter-var-root (var heap_sort_h) (constantly heap_sort_h)))) heap_sort_h)))

(defn heap_to_string [heap_to_string_h heap_to_string_heap_size]
  (binding [heap_to_string_i nil heap_to_string_s nil] (try (do (set! heap_to_string_s "[") (set! heap_to_string_i 0) (while (< heap_to_string_i heap_to_string_heap_size) (do (set! heap_to_string_s (str heap_to_string_s (mochi_str (nth heap_to_string_h heap_to_string_i)))) (when (< heap_to_string_i (- heap_to_string_heap_size 1)) (set! heap_to_string_s (str heap_to_string_s ", "))) (set! heap_to_string_i (+ heap_to_string_i 1)))) (set! heap_to_string_s (str heap_to_string_s "]")) (throw (ex-info "return" {:v heap_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_heap nil)

(def ^:dynamic main_size nil)

(def ^:dynamic main_m nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_heap) (constantly [103.0 9.0 1.0 7.0 11.0 15.0 25.0 201.0 209.0 107.0 5.0]))
      (alter-var-root (var main_size) (constantly (build_max_heap main_heap)))
      (println (heap_to_string main_heap main_size))
      (alter-var-root (var main_m) (constantly (let [__res (extract_max main_heap main_size)] (do (alter-var-root (var main_heap) (constantly extract_max_h)) __res))))
      (alter-var-root (var main_size) (constantly (- main_size 1)))
      (println (mochi_str main_m))
      (println (heap_to_string main_heap main_size))
      (alter-var-root (var main_size) (constantly (let [__res (insert main_heap main_size 100.0)] (do (alter-var-root (var main_heap) (constantly insert_h)) (alter-var-root (var main_size) (constantly insert_heap_size)) __res))))
      (println (heap_to_string main_heap main_size))
      (let [__res (heap_sort main_heap main_size)] (do (alter-var-root (var main_heap) (constantly heap_sort_h)) __res))
      (println (heap_to_string main_heap main_size))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
