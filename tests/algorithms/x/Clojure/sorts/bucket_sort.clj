(ns main (:refer-clojure :exclude [int_to_float floor_int set_at_float set_at_list_float sort_float bucket_sort_with_count bucket_sort]))

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

(declare int_to_float floor_int set_at_float set_at_list_float sort_float bucket_sort_with_count bucket_sort)

(def ^:dynamic bucket_sort_with_count_bucket nil)

(def ^:dynamic bucket_sort_with_count_bucket_size nil)

(def ^:dynamic bucket_sort_with_count_buckets nil)

(def ^:dynamic bucket_sort_with_count_i nil)

(def ^:dynamic bucket_sort_with_count_idx nil)

(def ^:dynamic bucket_sort_with_count_j nil)

(def ^:dynamic bucket_sort_with_count_max_value nil)

(def ^:dynamic bucket_sort_with_count_min_value nil)

(def ^:dynamic bucket_sort_with_count_result nil)

(def ^:dynamic bucket_sort_with_count_sorted_bucket nil)

(def ^:dynamic bucket_sort_with_count_val nil)

(def ^:dynamic floor_int_i nil)

(def ^:dynamic set_at_float_i nil)

(def ^:dynamic set_at_float_res nil)

(def ^:dynamic set_at_list_float_i nil)

(def ^:dynamic set_at_list_float_res nil)

(def ^:dynamic sort_float_i nil)

(def ^:dynamic sort_float_j nil)

(def ^:dynamic sort_float_key nil)

(def ^:dynamic sort_float_res nil)

(defn int_to_float [int_to_float_x]
  (try (throw (ex-info "return" {:v (* int_to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn floor_int [floor_int_x]
  (binding [floor_int_i nil] (try (do (set! floor_int_i 0) (while (<= (int_to_float (+ floor_int_i 1)) floor_int_x) (set! floor_int_i (+ floor_int_i 1))) (throw (ex-info "return" {:v floor_int_i}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_at_float [set_at_float_xs set_at_float_idx set_at_float_value]
  (binding [set_at_float_i nil set_at_float_res nil] (try (do (set! set_at_float_i 0) (set! set_at_float_res []) (while (< set_at_float_i (count set_at_float_xs)) (do (if (= set_at_float_i set_at_float_idx) (set! set_at_float_res (conj set_at_float_res set_at_float_value)) (set! set_at_float_res (conj set_at_float_res (nth set_at_float_xs set_at_float_i)))) (set! set_at_float_i (+ set_at_float_i 1)))) (throw (ex-info "return" {:v set_at_float_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_at_list_float [set_at_list_float_xs set_at_list_float_idx set_at_list_float_value]
  (binding [set_at_list_float_i nil set_at_list_float_res nil] (try (do (set! set_at_list_float_i 0) (set! set_at_list_float_res []) (while (< set_at_list_float_i (count set_at_list_float_xs)) (do (if (= set_at_list_float_i set_at_list_float_idx) (set! set_at_list_float_res (conj set_at_list_float_res set_at_list_float_value)) (set! set_at_list_float_res (conj set_at_list_float_res (nth set_at_list_float_xs set_at_list_float_i)))) (set! set_at_list_float_i (+ set_at_list_float_i 1)))) (throw (ex-info "return" {:v set_at_list_float_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_float [sort_float_xs]
  (binding [sort_float_i nil sort_float_j nil sort_float_key nil sort_float_res nil] (try (do (set! sort_float_res sort_float_xs) (set! sort_float_i 1) (while (< sort_float_i (count sort_float_res)) (do (set! sort_float_key (nth sort_float_res sort_float_i)) (set! sort_float_j (- sort_float_i 1)) (while (and (>= sort_float_j 0) (> (nth sort_float_res sort_float_j) sort_float_key)) (do (set! sort_float_res (set_at_float sort_float_res (+ sort_float_j 1) (nth sort_float_res sort_float_j))) (set! sort_float_j (- sort_float_j 1)))) (set! sort_float_res (set_at_float sort_float_res (+ sort_float_j 1) sort_float_key)) (set! sort_float_i (+ sort_float_i 1)))) (throw (ex-info "return" {:v sort_float_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bucket_sort_with_count [bucket_sort_with_count_xs bucket_sort_with_count_bucket_count]
  (binding [bucket_sort_with_count_bucket nil bucket_sort_with_count_bucket_size nil bucket_sort_with_count_buckets nil bucket_sort_with_count_i nil bucket_sort_with_count_idx nil bucket_sort_with_count_j nil bucket_sort_with_count_max_value nil bucket_sort_with_count_min_value nil bucket_sort_with_count_result nil bucket_sort_with_count_sorted_bucket nil bucket_sort_with_count_val nil] (try (do (when (or (= (count bucket_sort_with_count_xs) 0) (<= bucket_sort_with_count_bucket_count 0)) (throw (ex-info "return" {:v []}))) (set! bucket_sort_with_count_min_value (nth bucket_sort_with_count_xs 0)) (set! bucket_sort_with_count_max_value (nth bucket_sort_with_count_xs 0)) (set! bucket_sort_with_count_i 1) (while (< bucket_sort_with_count_i (count bucket_sort_with_count_xs)) (do (when (< (nth bucket_sort_with_count_xs bucket_sort_with_count_i) bucket_sort_with_count_min_value) (set! bucket_sort_with_count_min_value (nth bucket_sort_with_count_xs bucket_sort_with_count_i))) (when (> (nth bucket_sort_with_count_xs bucket_sort_with_count_i) bucket_sort_with_count_max_value) (set! bucket_sort_with_count_max_value (nth bucket_sort_with_count_xs bucket_sort_with_count_i))) (set! bucket_sort_with_count_i (+ bucket_sort_with_count_i 1)))) (when (= bucket_sort_with_count_max_value bucket_sort_with_count_min_value) (throw (ex-info "return" {:v bucket_sort_with_count_xs}))) (set! bucket_sort_with_count_bucket_size (/ (- bucket_sort_with_count_max_value bucket_sort_with_count_min_value) (int_to_float bucket_sort_with_count_bucket_count))) (set! bucket_sort_with_count_buckets []) (set! bucket_sort_with_count_i 0) (while (< bucket_sort_with_count_i bucket_sort_with_count_bucket_count) (do (set! bucket_sort_with_count_buckets (conj bucket_sort_with_count_buckets [])) (set! bucket_sort_with_count_i (+ bucket_sort_with_count_i 1)))) (set! bucket_sort_with_count_i 0) (while (< bucket_sort_with_count_i (count bucket_sort_with_count_xs)) (do (set! bucket_sort_with_count_val (nth bucket_sort_with_count_xs bucket_sort_with_count_i)) (set! bucket_sort_with_count_idx (floor_int (/ (- bucket_sort_with_count_val bucket_sort_with_count_min_value) bucket_sort_with_count_bucket_size))) (when (< bucket_sort_with_count_idx 0) (set! bucket_sort_with_count_idx 0)) (when (>= bucket_sort_with_count_idx bucket_sort_with_count_bucket_count) (set! bucket_sort_with_count_idx (- bucket_sort_with_count_bucket_count 1))) (set! bucket_sort_with_count_bucket (nth bucket_sort_with_count_buckets bucket_sort_with_count_idx)) (set! bucket_sort_with_count_bucket (conj bucket_sort_with_count_bucket bucket_sort_with_count_val)) (set! bucket_sort_with_count_buckets (set_at_list_float bucket_sort_with_count_buckets bucket_sort_with_count_idx bucket_sort_with_count_bucket)) (set! bucket_sort_with_count_i (+ bucket_sort_with_count_i 1)))) (set! bucket_sort_with_count_result []) (set! bucket_sort_with_count_i 0) (while (< bucket_sort_with_count_i (count bucket_sort_with_count_buckets)) (do (set! bucket_sort_with_count_sorted_bucket (sort_float (nth bucket_sort_with_count_buckets bucket_sort_with_count_i))) (set! bucket_sort_with_count_j 0) (while (< bucket_sort_with_count_j (count bucket_sort_with_count_sorted_bucket)) (do (set! bucket_sort_with_count_result (conj bucket_sort_with_count_result (nth bucket_sort_with_count_sorted_bucket bucket_sort_with_count_j))) (set! bucket_sort_with_count_j (+ bucket_sort_with_count_j 1)))) (set! bucket_sort_with_count_i (+ bucket_sort_with_count_i 1)))) (throw (ex-info "return" {:v bucket_sort_with_count_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bucket_sort [bucket_sort_xs]
  (try (throw (ex-info "return" {:v (bucket_sort_with_count bucket_sort_xs 10)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (bucket_sort [(- 1.0) 2.0 (- 5.0) 0.0])))
      (println (str (bucket_sort [9.0 8.0 7.0 6.0 (- 12.0)])))
      (println (str (bucket_sort [0.4 1.2 0.1 0.2 (- 0.9)])))
      (println (str (bucket_sort [])))
      (println (str (bucket_sort [(- 10000000000.0) 10000000000.0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
