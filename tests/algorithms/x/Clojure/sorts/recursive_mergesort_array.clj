(ns main (:refer-clojure :exclude [subarray merge]))

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

(declare subarray merge)

(def ^:dynamic merge_arr nil)

(def ^:dynamic merge_index nil)

(def ^:dynamic merge_left_array nil)

(def ^:dynamic merge_left_index nil)

(def ^:dynamic merge_left_size nil)

(def ^:dynamic merge_middle_length nil)

(def ^:dynamic merge_right_array nil)

(def ^:dynamic merge_right_index nil)

(def ^:dynamic merge_right_size nil)

(def ^:dynamic subarray_k nil)

(def ^:dynamic subarray_result nil)

(defn subarray [subarray_xs subarray_start subarray_end]
  (binding [subarray_k nil subarray_result nil] (try (do (set! subarray_result []) (set! subarray_k subarray_start) (while (< subarray_k subarray_end) (do (set! subarray_result (conj subarray_result (nth subarray_xs subarray_k))) (set! subarray_k (+ subarray_k 1)))) (throw (ex-info "return" {:v subarray_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge [merge_arr_p]
  (binding [merge_arr nil merge_index nil merge_left_array nil merge_left_index nil merge_left_size nil merge_middle_length nil merge_right_array nil merge_right_index nil merge_right_size nil] (try (do (set! merge_arr merge_arr_p) (when (> (count merge_arr) 1) (do (set! merge_middle_length (/ (count merge_arr) 2)) (set! merge_left_array (subarray merge_arr 0 merge_middle_length)) (set! merge_right_array (subarray merge_arr merge_middle_length (count merge_arr))) (set! merge_left_size (count merge_left_array)) (set! merge_right_size (count merge_right_array)) (merge merge_left_array) (merge merge_right_array) (set! merge_left_index 0) (set! merge_right_index 0) (set! merge_index 0) (while (and (< merge_left_index merge_left_size) (< merge_right_index merge_right_size)) (do (if (< (nth merge_left_array merge_left_index) (nth merge_right_array merge_right_index)) (do (set! merge_arr (assoc merge_arr merge_index (nth merge_left_array merge_left_index))) (set! merge_left_index (+ merge_left_index 1))) (do (set! merge_arr (assoc merge_arr merge_index (nth merge_right_array merge_right_index))) (set! merge_right_index (+ merge_right_index 1)))) (set! merge_index (+ merge_index 1)))) (while (< merge_left_index merge_left_size) (do (set! merge_arr (assoc merge_arr merge_index (nth merge_left_array merge_left_index))) (set! merge_left_index (+ merge_left_index 1)) (set! merge_index (+ merge_index 1)))) (while (< merge_right_index merge_right_size) (do (set! merge_arr (assoc merge_arr merge_index (nth merge_right_array merge_right_index))) (set! merge_right_index (+ merge_right_index 1)) (set! merge_index (+ merge_index 1)))))) (throw (ex-info "return" {:v merge_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (merge [10 9 8 7 6 5 4 3 2 1])))
      (println (str (merge [1 2 3 4 5 6 7 8 9 10])))
      (println (str (merge [10 22 1 2 3 9 15 23])))
      (println (str (merge [100])))
      (println (str (merge [])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
