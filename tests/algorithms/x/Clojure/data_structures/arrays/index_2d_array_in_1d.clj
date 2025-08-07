(ns main (:refer-clojure :exclude [iterator_values index_2d_array_in_1d]))

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

(declare iterator_values index_2d_array_in_1d)

(def ^:dynamic index_2d_array_in_1d_cols nil)

(def ^:dynamic index_2d_array_in_1d_rows nil)

(def ^:dynamic iterator_values_result nil)

(defn iterator_values [iterator_values_matrix]
  (binding [iterator_values_result nil] (try (do (set! iterator_values_result []) (doseq [row iterator_values_matrix] (doseq [value row] (set! iterator_values_result (conj iterator_values_result value)))) (throw (ex-info "return" {:v iterator_values_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_2d_array_in_1d [index_2d_array_in_1d_array index_2d_array_in_1d_index]
  (binding [index_2d_array_in_1d_cols nil index_2d_array_in_1d_rows nil] (try (do (set! index_2d_array_in_1d_rows (count index_2d_array_in_1d_array)) (set! index_2d_array_in_1d_cols (count (nth index_2d_array_in_1d_array 0))) (when (or (= index_2d_array_in_1d_rows 0) (= index_2d_array_in_1d_cols 0)) (throw (Exception. "no items in array"))) (when (or (< index_2d_array_in_1d_index 0) (>= index_2d_array_in_1d_index (* index_2d_array_in_1d_rows index_2d_array_in_1d_cols))) (throw (Exception. "index out of range"))) (throw (ex-info "return" {:v (nth (nth index_2d_array_in_1d_array (Integer/parseInt (/ index_2d_array_in_1d_index index_2d_array_in_1d_cols))) (mod index_2d_array_in_1d_index index_2d_array_in_1d_cols))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (iterator_values [[5] [(- 523)] [(- 1)] [34] [0]])))
      (println (str (iterator_values [[5 (- 523) (- 1)] [34 0]])))
      (println (str (index_2d_array_in_1d [[0 1 2 3] [4 5 6 7] [8 9 10 11]] 5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
